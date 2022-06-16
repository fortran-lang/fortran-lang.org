import yaml
from pathlib import Path
print("learn section")
conf = yaml.safe_load(Path('learn.yml').read_text())
print(conf)
