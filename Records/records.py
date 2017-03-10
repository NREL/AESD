"""
Design Studio Python Records API

Created by: Michael Rossol Feb. 2017
"""
import asyncio
import numpy as np
import pandas as pd
import records_def_4_pb2 as proto
import websockets

currentID = 0

__all__ = ['next_ID', ]


def next_ID():
    """
    Creates new ID from global currentID counter
    Parameters
    ----------

    Returns
    -------
    currentID : 'int'
        New ID
    """
    global currentID
    currentID += 1
    return currentID
