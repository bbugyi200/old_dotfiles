{% INSERT %}

import datetime as dt
import sys  # noqa
import os  # noqa
import unittest.mock as mock

import pytest  # noqa

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

import {{ PY_MODULE }}
