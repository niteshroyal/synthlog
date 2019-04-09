from setuptools import setup

setup(
    name="synthlog",
    version="0.0",
    description="",
    url="",
    author="",
    author_email="",
    license="",
    packages=["synthlog"],
    install_requires=["problog", "openpyxl", "problog"],
    entry_points={"console_scripts": ["synthlog=synthlog.__main__:main"]},
    zip_safe=False,
)
