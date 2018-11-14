{% START INSERT MODE %}

import datetime as dt
import importlib.util
import importlib.machinery
import os
import unittest.mock as mock

loader = importlib.machinery.SourceFileLoader("{{ SCRIPT }}", "/home/bryan/Dropbox/bin/main/{{ SCRIPT }}")
spec = importlib.util.spec_from_loader("{{ SCRIPT }}", loader)
S = importlib.util.module_from_spec(spec)
spec.loader.exec_module(S)

import pytest  # noqa
