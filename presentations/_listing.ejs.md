<% const years = [...new Set(items.map(item => item.year))]; %>

```{=html}
<style>
/* Keep a heading with every entry so Quarto's category filters can remove
   any entry. Show only the first heading for each year still in the list. */
<% for (const year of years) { %>
#listing-presentations .presentation-entry[data-year="<%- year %>"] + .presentation-entry[data-year="<%- year %>"] > .presentation-year {
  display: none;
}
<% } %>
</style>

<div class="list quarto-listing-container-default">
<% for (const item of items) { %>
  <section class="presentation-entry" data-year="<%- item.year %>" <%= metadataAttrs(item) %>>
    <h2 class="presentation-year"><%- item.year %></h2>
    <div class="quarto-post image-right">
      <div class="thumbnail">
        <a href="<%- item.path %>" class="no-external">
          <img src="<%- item.image %>" alt="<%- item['image-alt'] || '' %>" class="thumbnail-image" loading="lazy" style="height: 160px;">
        </a>
      </div>
      <div class="body">
        <h3 class="no-anchor listing-title"><a href="<%- item.path %>" class="no-external"><%- item.title %></a></h3>
        <% if (item.subtitle) { %>
        <div class="listing-subtitle"><a href="<%- item.path %>" class="no-external"><%- item.subtitle %></a></div>
        <% } %>
        <div class="listing-categories">
          <% for (const category of item.categories || []) { %>
          <div class="listing-category" onclick="window.quartoListingCategory('<%= utils.b64encode(category) %>'); return false;"><%- category %></div>
          <% } %>
        </div>
        <div class="delink listing-description"><a href="<%- item.path %>" class="no-external">
```

<%= item.description %>

```{=html}
        </a></div>
      </div>
      <div class="metadata"></div>
    </div>
  </section>
<% } %>
</div>
```
