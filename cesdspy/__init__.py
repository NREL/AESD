__title__ = 'cesdspy'
__version__ = '0.1.0'
__author__ = 'Nick Wunder'

import servers
import models
import records
import filters
import bookmarks
import work

# Set default logging handler to avoid "No handler found" warnings.
import logging
try:  # Python 2.7+
    from logging import NullHandler
except ImportError:
    class NullHandler(logging.Handler):
        def emit(self, record):
            pass

logging.getLogger(__name__).addHandler(NullHandler())
