import setuptools

with open("README.md", "r") as fh:
    long_description = fh.read()

setuptools.setup(
    name="evalpy",
    version="0.0.1",
    author="David Rother",
    author_email="david@edv-drucksysteme.de",
    description="A lightweight framework to ",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/DavidRother/evalpy",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3.7",
        "License :: OSI Approved :: GNU GENERAL PUBLIC LICENSE",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6',
)
