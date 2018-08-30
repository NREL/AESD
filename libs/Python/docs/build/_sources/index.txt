.. cesdspy documentation master file, created by
   sphinx-quickstart on Wed Aug 31 11:51:27 2016.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Behold CESDSpy's documentation!
================================================================================

A Python client for Clean Energy Systems Design Studio (CESDS) servers.

Install
--------------------------------------------------------------------------------
.. code-block:: shell

  pip install git+https://github.nrel.gov/nwunder2/cesdspy

Use
--------------------------------------------------------------------------------
.. code-block:: python

  >>> import cesdspy
  >>> url = 'http://1lv11lamb01.nrel.gov:8090'
  >>> models = cesdspy.models.list(url)
  >>> records = cesdspy.records.list(url, models[0])
  >>> print(records)
  [{u'variables': [{u'id': u'NtA', u'value': u'vIf9'}], u'id': u'Il81Na1b665QiXyF'}]

.. module:: cesdspy

Contents
--------

.. toctree::
   :maxdepth: 2

   cesdspy
   logging

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
