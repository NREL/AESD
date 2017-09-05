from distutils.core import setup

setup(
    name='aesd-records',
    version='0.1.0',
    author='Michael Rossol',
    author_email='michael.rossol@nrel.gov',
    packages=['aesd-records', ],
    scripts=['cli.py', ],
    url='https://github.nrel.gov/d-star/python-records',
    description='The Python client for version 3 of the AESD Records API.',
)
