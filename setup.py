from setuptools import setup, find_packages

setup(
    name='cesdspy',
    version='0.1.0',
    description='A CESDS Python client',
    url='https://github.nrel.gov/nwunder2/cesdspy',
    author='Nick Wunder',
    author_email='nick.wunder@nrel.gov',
    packages=find_packages(),
    install_requires=['requests'],
    extras_require={
        'dev': ['Sphinx', 'sphinxcontrib-httpdomain']
    }
)
