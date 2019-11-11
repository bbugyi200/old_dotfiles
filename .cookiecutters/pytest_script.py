{% INSERT %}

import datetime as dt
import importlib.util
import importlib.machinery
import sys  # noqa
import os  # noqa
import unittest.mock as mock

loader = importlib.machinery.SourceFileLoader("{{ SCRIPT }}", "/home/bryan/Sync/bin/main/{{ SCRIPT }}")
spec = importlib.util.spec_from_loader("{{ SCRIPT }}", loader)
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)

import pytest  # noqa
