import yaml
from pathlib import Path
print("learn section")
conf = yaml.safe_load(Path('_data/learning.yml').read_text())
print(conf)