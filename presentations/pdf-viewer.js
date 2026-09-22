// PDF.js is loaded only on pages with a viewer. Keep the library and worker
// on the same pinned release; the PDFs themselves are served by this site.
const pdfjsBase = "https://cdn.jsdelivr.net/npm/pdfjs-dist@6.3.289/";
const viewers = document.querySelectorAll(".pdf-viewer[data-pdf]");

if (viewers.length) {
  const library = import(`${pdfjsBase}build/pdf.min.mjs`);
  viewers.forEach((viewer) => initialiseViewer(viewer, library));
}

async function initialiseViewer(viewer, library) {
  const isPoster = viewer.dataset.mode === "poster";
  viewer.tabIndex = 0;
  viewer.setAttribute("role", "region");
  viewer.innerHTML = `
    <div class="pdf-viewer-viewport" tabindex="0" aria-label="${isPoster ? "Poster; scroll when zoomed" : "Slides"}"></div>
    <div class="pdf-viewer-controls" role="group" aria-label="${isPoster ? "Poster zoom" : "Slide navigation"}">
      ${isPoster ? `
        <button type="button" class="btn btn-outline-primary btn-sm" data-action="zoom-out" aria-label="Zoom out" disabled>−</button>
        <button type="button" class="btn btn-outline-primary btn-sm" data-action="fit" disabled>Fit</button>
        <button type="button" class="btn btn-outline-primary btn-sm" data-action="zoom-in" aria-label="Zoom in" disabled>+</button>
      ` : `
        <button type="button" class="btn btn-outline-primary btn-sm" data-action="previous" aria-label="Previous slide" disabled>← Previous</button>
        <span class="pdf-viewer-position" aria-live="polite"></span>
        <button type="button" class="btn btn-outline-primary btn-sm" data-action="next" aria-label="Next slide" disabled>Next →</button>
      `}
    </div>
    <p class="pdf-viewer-status" role="status">Loading ${isPoster ? "poster" : "slides"}…</p>
    <div class="pdf-viewer-text visually-hidden"></div>
  `;

  const viewport = viewer.querySelector(".pdf-viewer-viewport");
  const status = viewer.querySelector(".pdf-viewer-status");
  const position = viewer.querySelector(".pdf-viewer-position");
  const pageText = viewer.querySelector(".pdf-viewer-text");
  const buttons = viewer.querySelectorAll("button[data-action]");
  let pdf;
  let pageNumber = 1;
  let zoom = 1;
  let request = 0;
  let renderTask;

  function updateButtons() {
    for (const button of buttons) {
      button.disabled = !pdf || {
        previous: pageNumber === 1,
        next: pageNumber === pdf?.numPages,
        "zoom-out": zoom === 1,
        "zoom-in": zoom === 4,
        fit: zoom === 1,
      }[button.dataset.action];
    }
  }

  async function renderPage() {
    const currentRequest = ++request;
    const currentPage = pageNumber;
    renderTask?.cancel();
    viewer.setAttribute("aria-busy", "true");
    status.textContent = "Loading…";
    updateButtons();

    try {
      const page = await pdf.getPage(currentPage);
      if (currentRequest !== request) return;
      const original = page.getViewport({ scale: 1 });
      // Measure the outer box, so scrollbars from the previous zoom do not
      // alter the fit scale. Leave room for a scrollbar when zooming in.
      const width = viewport.offsetWidth - (zoom > 1 ? 16 : 0);
      const height = viewport.offsetHeight;
      const fitScale = Math.min(width / original.width, height / original.height);
      const pageViewport = page.getViewport({ scale: fitScale * zoom });
      // Limit the backing canvas on high-density screens and large posters.
      const pixelRatio = Math.min(window.devicePixelRatio || 1, 2,
        8192 / pageViewport.width, 8192 / pageViewport.height);
      const canvas = document.createElement("canvas");
      canvas.width = Math.floor(pageViewport.width * pixelRatio);
      canvas.height = Math.floor(pageViewport.height * pixelRatio);
      canvas.style.width = `${pageViewport.width}px`;
      canvas.style.height = `${pageViewport.height}px`;
      canvas.setAttribute("role", "img");
      canvas.setAttribute("aria-label", isPoster ? "Research poster" : `Slide ${currentPage} of ${pdf.numPages}`);
      renderTask = page.render({
        canvasContext: canvas.getContext("2d"),
        viewport: pageViewport,
        transform: [pixelRatio, 0, 0, pixelRatio, 0, 0],
      });
      await renderTask.promise;
      const text = await page.getTextContent();
      if (currentRequest !== request) return;

      // Replace the displayed canvas only after the new page is ready.
      viewport.replaceChildren(canvas);
      pageText.textContent = text.items.map((item) => item.str).join(" ");
      if (position) position.textContent = `${currentPage} / ${pdf.numPages}`;
      status.textContent = "";
      if (zoom === 1) viewport.scrollTo(0, 0);
    } catch (error) {
      if (currentRequest !== request || error.name === "RenderingCancelledException") return;
      status.textContent = "This page could not be displayed. Please reload to try again.";
      console.error("PDF page rendering failed:", error);
    } finally {
      if (currentRequest === request) viewer.setAttribute("aria-busy", "false");
    }
  }

  function changePage(nextPage) {
    if (!pdf) return;
    const boundedPage = Math.max(1, Math.min(pdf.numPages, nextPage));
    if (boundedPage === pageNumber) return;
    pageNumber = boundedPage;
    renderPage();
  }

  viewer.addEventListener("click", (event) => {
    const button = event.target.closest("button[data-action]");
    if (!button || button.disabled || !pdf) return;
    switch (button.dataset.action) {
      case "previous": changePage(pageNumber - 1); return;
      case "next": changePage(pageNumber + 1); return;
      case "zoom-in": zoom = Math.min(4, zoom + 0.5); break;
      case "zoom-out": zoom = Math.max(1, zoom - 0.5); break;
      case "fit": zoom = 1; break;
    }
    renderPage();
  });

  viewer.addEventListener("keydown", (event) => {
    if (isPoster || !pdf || event.altKey || event.ctrlKey || event.metaKey) return;
    const nextPage = { ArrowLeft: pageNumber - 1, ArrowRight: pageNumber + 1, Home: 1, End: pdf.numPages }[event.key];
    if (nextPage === undefined) return;
    event.preventDefault();
    changePage(nextPage);
  });

  try {
    const pdfjs = await library;
    pdfjs.GlobalWorkerOptions.workerSrc = `${pdfjsBase}build/pdf.worker.min.mjs`;
    pdf = await pdfjs.getDocument({
      url: new URL(viewer.dataset.pdf, document.baseURI).href,
      cMapUrl: `${pdfjsBase}cmaps/`,
      standardFontDataUrl: `${pdfjsBase}standard_fonts/`,
      wasmUrl: `${pdfjsBase}wasm/`,
      iccUrl: `${pdfjsBase}iccs/`,
    }).promise;
    await renderPage();
    // Redraw from the PDF when the page or device size changes.
    let resizeTimer;
    const observer = new ResizeObserver(() => {
      clearTimeout(resizeTimer);
      resizeTimer = setTimeout(renderPage, 120);
    });
    observer.observe(viewport);
  } catch (error) {
    status.textContent = "The presentation could not be loaded. Check your connection and reload to try again.";
    console.error("PDF loading failed:", error);
  }
}
