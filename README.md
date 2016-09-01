cesdspy
================================================================================

## Develop

> Prerequisites
> * virtualenv

Initial Setup
* `virtualenv .env`
* `source .evn/bin/activate`
* `pip install -r requirements.txt`

Setup
* `source .evn/bin/activate`
* `pip install -e .[dev,test]`
* `pip freeze > requirements.txt`
* `deactivate`

Build docs
`sphinx-build -b html docs/source docs/build`
