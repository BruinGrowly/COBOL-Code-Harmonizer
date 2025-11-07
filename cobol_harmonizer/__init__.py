"""
COBOL Code Harmonizer

A semantic analysis tool for COBOL codebases using the LJPW framework
(Love, Justice, Power, Wisdom) to detect when procedure names contradict
their implementations.
"""

__version__ = '0.1.0'
__author__ = 'COBOL Code Harmonizer Contributors'
__license__ = 'MIT'

from cobol_harmonizer.semantic.disharmony import DisharmonyCalculator

__all__ = ['DisharmonyCalculator']
