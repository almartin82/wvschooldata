"""
Tests for pywvschooldata Python wrapper.

Minimal smoke tests - the actual data logic is tested by R testthat.
These just verify the Python wrapper imports and exposes expected functions.
"""

import pytest


def test_import_package():
    """Package imports successfully."""
    import pywvschooldata
    assert pywvschooldata is not None


def test_has_fetch_enr():
    """fetch_enr function is available."""
    import pywvschooldata
    assert hasattr(pywvschooldata, 'fetch_enr')
    assert callable(pywvschooldata.fetch_enr)


def test_has_get_available_years():
    """get_available_years function is available."""
    import pywvschooldata
    assert hasattr(pywvschooldata, 'get_available_years')
    assert callable(pywvschooldata.get_available_years)


def test_has_version():
    """Package has a version string."""
    import pywvschooldata
    assert hasattr(pywvschooldata, '__version__')
    assert isinstance(pywvschooldata.__version__, str)
