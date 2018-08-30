.. aesdpy documentation master file, created by
   sphinx-quickstart on Wed Aug 31 11:51:27 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Behold AESDpy's documentation!
================================================================================

A Python client for Clean Energy Systems Design Studio (AESD) servers.

Install
--------------------------------------------------------------------------------
.. code-block:: shell

  pip install git+https://github.nrel.gov/nwunder2/aesdpy

Use
--------------------------------------------------------------------------------
.. code-block:: python

  >>> import aesdpy
  >>> url = 'http://1lv11lamb01.nrel.gov:8090'
  >>> models = aesdpy.models.list(url)
  >>> records = aesdpy.records.list(url, models[0])
  >>> print(records)
  [{u'variables': [{u'id': u'NtA', u'value': u'vIf9'}], u'id': u'Il81Na1b665QiXyF'}]

.. module:: aesdpy

Contents
--------

.. toctree::
   :maxdepth: 2

   aesdpy
   logging

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
