"""
Package for models request

Created by: Michael Rossol Sept. 2017
"""
from .errors import ProtoError
from . import records_def_4_pb2 as proto

__all__ = ['set_value', 'Filter_Variable', 'Filter']


def set_value(value):
    """
    Convert value into proto.Value based on value type

    Parameters
    ----------
    value : 'int' | 'float' | 'string'
        value to be assigned to proto message

    Returns
    ---------
    proto_value : 'proto.Value'
        Protobuff Value message based on value type
    """
    proto_value = proto.Value()
    if isinstance(value, int):
        proto_value.integer_value = value
    elif isinstance(value, float):
        proto_value.real_value = value
    elif isinstance(value, str):
        proto_value.string_value = value
    else:
        raise ProtoError('{:} is {:}, must be int, float, or str'
                         .format(value, type(value)))

    return proto_value


class Filter(object):
    """
    Filter class to handle filter expressions
    """
    def __init__(self, filter_domain):
        """
        Initialized Filter object

        Parameters
        ----------
        filter_domain : 'Filter' | 'Filter_Variable'
            Filter_Variable or Filter object
        """
        self.filter = filter_domain

    def __and__(self, other):
        """
        Create Filter Union message using & symbol

        Parameters
        ----------
        other : 'Filter' | 'Filter_Variable'
            Filter_Variable or Filter object to be combined with self

        Returns
        ---------
        self : 'Filter'
            Updated Filter object
        """
        union = proto.FilterExpression()
        domains = [self.filter, other.filter]
        union.filter_union.filter_expressions.extend(domains)
        self.filter = union
        return self

    def __or__(self, other):
        """
        Create Filter Instersection message using | symbol

        Parameters
        ----------
        other : 'Filter' | 'Filter_Variable'
            Filter_Variable or Filter object to be combined with self

        Returns
        ---------
        self : 'Filter'
            Updated Filter object
        """
        intersection = proto.FilterExpression()
        domains = [self.filter, other.filter]
        intersection.filter_intersection.filter_expressions.extend(domains)
        self.filter = intersection
        return self


class Filter_Variable(Filter):
    """
    Sub-class to handle filter domains
    """
    def __init__(self, var_id):
        """
        Initialized Filter object

        Parameters
        ----------
        var_id : 'int'
            Id of variable to be filtered
        """
        self.filter = proto.FilterExpression()
        self.filter.filter_domain.var_id = var_id
        super().__init__(self.filter)

    def __invert__(self):
        """
        Create Filter Not message using ~ symbol

        Returns
        ---------
        self : 'Filter'
            Updated Filter object
        """
        not_filter = proto.FilterExpression()
        not_filter.filter_not.filter_expression.MergeFrom(self.filter)
        self.filter = not_filter
        return self

    def __le__(self, value):
        """
        Create closed Filter Domain message with interval [None, value]
        uses <= symbol

        Parameters
        ----------
        value : 'int' | 'float' | 'str'
            Filter domain value

        Returns
        ---------
        self : 'Filter'
            Updated Filter object
        """
        value = set_value(value)
        self.filter.filter_domain.interval.last_value.MergeFrom(value)
        return self

    def __ge__(self, value):
        """
        Create closed Filter Domain message with interval [value, None]
        uses >= symbol

        Parameters
        ----------
        value : 'int' | 'float' | 'str'
            Filter domain value

        Returns
        ---------
        self : 'Filter'
            Updated Filter object
        """
        value = set_value(value)
        self.filter.filter_domain.interval.first_value.MergeFrom(value)
        return self

    def __lt__(self, value):
        """
        Create open Filter Domain message with interval (None, value)
        uses < symbol

        Parameters
        ----------
        value : 'int' | 'float' | 'str'
            Filter domain value

        Returns
        ---------
        self : 'Filter'
            Updated Filter object
        """
        self = self.__le__(value)
        return self.__invert__()

    def __gt__(self, value):
        """
        Create open Filter Domain message with interval (value, None)
        uses > symbol

        Parameters
        ----------
        value : 'int' | 'float' | 'str'
            Filter domain value

        Returns
        ---------
        self : 'Filter'
            Updated Filter object
        """
        self = self.__ge__(value)
        return self.__invert__()

    def __eq__(self, values):
        """
        Create Filter Domain message containing set of values
        uses == symbol

        Parameters
        ----------
        value : 'list' | 'tuple'
            Values in Filter Domain

        Returns
        ---------
        self : 'Filter'
            Updated Filter object
        """
        set_values = [set_value(val) for val in values]
        self.filter.filter_domain.set.elements.extend(set_values)
        return self

    def __ne__(self, values):
        """
        Create Filter Domain message excluding set of values
        uses != symbol

        Parameters
        ----------
        value : 'list' | 'tuple'
            Values for Filter Domain to exclude

        Returns
        ---------
        self : 'Filter'
            Updated Filter object
        """
        self = self.__eq__(values)
        return self.__invert__()
