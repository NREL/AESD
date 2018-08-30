Advanced Energy System Design (AESD) Records API
================================================


Visit NREL's [home page for Advanced Energy System Design (AESD)](https://www.nrel.gov/technical-assistance/advanced-energy-systems-design.html).

*   [Documentation](#documentation)
*   [Software](#software)


Abstract
--------

The Records API (application program interface) for Advanced Energy System Design (AESD) enables software that serves multidimensional record-oriented data to interoperate with software than uses such data. In the context of the Records API, multidimensional data records are simply tuples of real numbers, integers, and character strings, where each data value is tagged by a variable name, according to a pre-defined schema, and each record is assigned a unique integer identifier. Conceptually, these records are isomorphic to rows in a relational database, JSON objects, or key-value maps. Records servers might supply static datasets, sensor measurements that periodically update as new telemetry become available, or the results of simulations as the simulations generate new output. Records client software might display or analyze the data, but in the case of simulations the client request the creation of new ensembles for specified input parameters. It is also possible to chain records clients and servers together so that a client consuming data from a server might transform that data and serve it to additional clients.

This minimalist API avoids imposing burdensome metadata, structural, or implementation requirements on developers by relying on open-source technologies that are readily available for common programming languages. In particular, the API has been designed to place the least possible burden on services that provide data. This document defines the message format for the Records API, a transport mechanism for communicating the data, and the semantics for interpreting it. The message format is specified as Google Protocol Buffers and the transport mechanism uses WebSockets.  We discuss three major use cases for serving and consuming records data: (i) static data, (ii) dynamically augmented data, (iii) on-demand simulations, (iv) with filters, and (v) with bookmarks.  Separate implementations of the API exist in C++, Haskell, JavaScript, Python, and R.

Documentation
-------------

Technical Manual:
*   Latest: [[browser]](https://nrel.github.io/AESD/aesd-manual.html) [[Word]](https://nrel.github.io/AESD/aesd-manual.docx) [[PDF]](https://nrel.github.io/AESD/aesd-manual.pdf)
*   August 2018: [[PDF]](https://www.nrel.gov/docs/fy2018osti/68924.pdf)


Software
--------

*   [Protocol Buffers 3](https://developers.google.com/protocol-buffers/docs/proto3)
    *   [proto3 definition file](doc-src/aesd_records_4.proto)
*   Haskell
    *   Core library: [aesd-records](libs/Haskell/records/)
    *   [HDBC](http://hackage.haskell.org/package/HDBC) support: [aesd-records-hdbc](libs/Haskell/records-hdbc/)
    *   [Project Haystack](https://project-haystack.org/) support: [aesd-records-haystack](libs/Haskell/records-haystack/)
