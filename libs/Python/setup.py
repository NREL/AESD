from setuptools import setup, find_packages

setup(
    name='aesdpy',
    version='0.2.0',
    description='A AESD Python client',
    url='https://github.nrel.gov/nwunder2/aesdpy',
    author='Nick Wunder',
    author_email='nick.wunder@nrel.gov',
    packages=find_packages(),
    install_requires=['requests'],
    extras_require={
        'dev': ['Sphinx', 'sphinxcontrib-httpdomain']
    }
)
