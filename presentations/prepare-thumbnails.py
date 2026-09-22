"""Regenerate listing thumbnails after adding or replacing presentation PDFs.

Run: python3 presentations/prepare-thumbnails.py
(requires pdftoppm (Poppler))
"""

from pathlib import Path
import subprocess


for pdf in sorted(Path(__file__).resolve().parent.glob("assets/*/*.pdf")):
    thumbnail = pdf.with_suffix("")
    subprocess.run(
        [
            "pdftoppm",
            "-f", "1",
            "-singlefile",
            "-png",
            "-scale-to", "800",
            str(pdf),
            str(thumbnail),
        ],
        check=True,
    )
    print(f"Created {thumbnail.with_suffix('.png')}")
