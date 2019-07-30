from pathlib import Path 

path = Path(".")

for file in path.iterdir():
	if file.suffix == ".mp3":
		if file.stem[-1] == "1":
			file.rename(Path(file.stem[:-1] + ".mp3"))
		elif file.stem[-1].isdigit():
			file.unlink()