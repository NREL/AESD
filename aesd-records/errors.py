"""
Package for custom errors

Created by: Michael Rossol Feb. 2017
"""

__all__ = ['TimeoutError', 'ProtoError']


class TimeoutError(Exception):
    """
    Custom Error for Timeout
    """
    pass


class ProtoError(Exception):
    """
    Custom Error for Proto Messages
    """
    pass
