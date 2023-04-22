from pathlib import Path
from typing import  Dict
import yaml


root: Path = Path(__file__).parent
outdir: Path = root / "_site" 

with open(root / "_data" / "redirects.yml", "r", encoding="utf-8") as fd:
    all_redirects = yaml.safe_load(fd)


template = """
<!DOCTYPE HTML>
 
<meta charset="UTF-8">
<meta http-equiv="refresh" content="1; url={0}">
 
<script>
  window.location.href = "{0}"
</script>
 
<title>Page Redirection</title>

If you are not redirected automatically, follow the <a href='{0}'>link</a>.
"""

def build_redirects(redirects: Dict[str, str]) -> None:
    """
    Build the redirects for a single language.
    Parameters
    ----------
    redirects : Dict[str, str]
        Page redirects to build.
    """

    for source, target in redirects.items():
        source_path = outdir / source
        redirect = template.format(target)
        if not source_path.parent.exists():
            source_path.parent.mkdir(parents=True)
        with open(source_path, "w", encoding="utf-8") as fp:
            fp.write(redirect)

build_redirects(all_redirects)