Logging
================================================================================

Logging is disabled by default. To enable . . .

. . . to console

.. code-block:: python

  import aesdpy
  import logging

  logging.basicConfig(
      level=logging.INFO,
      format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
      datefmt='%m/%d/%Y %H:%M:%S')

  print(server.get('http://1lv11lamb01.nrel.gov:8090'))

. . . to file

.. code-block:: python

  import aesdpy
  import logging

  logging.basicConfig(
      level=logging.INFO,
      filename='aesdpy.log',
      format='%(asctime)s [%(levelname)s] %(name)s: %(message)s',
      datefmt='%m/%d/%Y %H:%M:%S')

  print(server.get('http://1lv11lamb01.nrel.gov:8090'))


Example output
--------------------------------------------------------------------------------

successful request
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  .. code-block:: shell

    10/15/2015 12:43:25 [INFO] requests.packages.urllib3.connectionpool: Starting new HTTP connection (1): 1lv11lamb01.nrel.gov
    10/15/2015 12:43:25 [DEBUG] requests.packages.urllib3.connectionpool: "GET / HTTP/1.1" 200 None

failed request
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

  .. code-block:: shell

    10/26/1985 01:21:00 [INFO] requests.packages.urllib3.connectionpool: Starting new HTTP connection (1): 1lv11lamb01.nrel.gov
    10/26/1985 01:21:00 [DEBUG] requests.packages.urllib3.connectionpool: "POST / HTTP/1.1" 400 None
    10/26/1985 01:21:00 [ERROR] aesdpy.servers:  POST http://1lv11lamb01.nrel.gov:8090/command 400 - invalid request
