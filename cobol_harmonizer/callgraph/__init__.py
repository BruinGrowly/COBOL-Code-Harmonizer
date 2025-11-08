"""
Call Graph Analysis for COBOL Code Harmonizer

This module analyzes program dependencies and call relationships:
- CALL statements (inter-program calls)
- PERFORM statements (intra-program calls)
- Dependency graphs and impact analysis
- Ripple effect calculations
"""

from .models import (
    CallSite,
    CallType,
    CallGraph,
    GraphNode,
    NodeType,
    NodeMetrics,
    GraphEdge,
    ImpactAnalysis,
)

from .extractor import CallExtractor
from .builder import CallGraphBuilder
from .analyzer import CallGraphAnalyzer

__all__ = [
    # Models
    "CallSite",
    "CallType",
    "CallGraph",
    "GraphNode",
    "NodeType",
    "NodeMetrics",
    "GraphEdge",
    "ImpactAnalysis",
    # Components
    "CallExtractor",
    "CallGraphBuilder",
    "CallGraphAnalyzer",
]
