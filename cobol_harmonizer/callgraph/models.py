"""
Data models for call graph analysis
"""

from dataclasses import dataclass, field
from typing import List, Dict, Set, Optional
from enum import Enum


class CallType(Enum):
    """Type of call relationship"""

    PROGRAM_CALL = "CALL"  # Inter-program (CALL 'SUBPROG')
    PROCEDURE_PERFORM = "PERFORM"  # Intra-program (PERFORM CALC-TOTAL)
    SECTION_PERFORM = "PERFORM-SECTION"  # Section call
    DYNAMIC_CALL = "CALL-DYNAMIC"  # CALL using variable


class NodeType(Enum):
    """Type of node in call graph"""

    PROGRAM = "program"
    PROCEDURE = "procedure"
    SECTION = "section"
    PARAGRAPH = "paragraph"


@dataclass
class CallSite:
    """
    A single call location in source code

    Represents where a call occurs (caller → callee)
    """

    caller: str  # Name of caller (procedure or program)
    callee: str  # Name of callee (what's being called)
    call_type: CallType
    line_number: int
    source_file: Optional[str] = None
    is_dynamic: bool = False  # True if using variable (e.g., CALL WS-PROGRAM-NAME)

    def __str__(self) -> str:
        dynamic = " (dynamic)" if self.is_dynamic else ""
        return f"{self.caller} → {self.callee} [{self.call_type.value}]{dynamic} at line {self.line_number}"


@dataclass
class NodeMetrics:
    """
    Metrics for a graph node

    Used to assess complexity and risk
    """

    fan_in: int = 0  # How many nodes call this one
    fan_out: int = 0  # How many nodes this one calls
    depth: int = 0  # Distance from entry point
    complexity: int = 0  # Cyclomatic complexity
    lines_of_code: int = 0

    def __str__(self) -> str:
        return (
            f"Metrics(fan_in={self.fan_in}, fan_out={self.fan_out}, depth={self.depth})"
        )


@dataclass
class GraphNode:
    """
    Node in call graph

    Represents a program, procedure, or section
    """

    id: str  # Unique identifier
    name: str  # Display name
    node_type: NodeType
    file_path: Optional[str] = None
    start_line: int = 0
    end_line: int = 0
    metrics: NodeMetrics = field(default_factory=NodeMetrics)

    # Harmony metrics (from semantic analysis)
    harmony_rate: Optional[float] = None
    disharmony_score: Optional[float] = None

    def __str__(self) -> str:
        return f"{self.node_type.value.upper()}: {self.name}"

    def __hash__(self) -> int:
        return hash(self.id)


@dataclass
class GraphEdge:
    """
    Edge in call graph (represents a call relationship)
    """

    source: str  # Caller node ID
    target: str  # Callee node ID
    call_type: CallType
    call_count: int = 1  # How many times this call occurs
    line_numbers: List[int] = field(default_factory=list)

    def __str__(self) -> str:
        return f"{self.source} → {self.target} ({self.call_count}x)"


@dataclass
class CallGraph:
    """
    Complete call graph for a program or codebase
    """

    nodes: Dict[str, GraphNode] = field(default_factory=dict)
    edges: List[GraphEdge] = field(default_factory=list)

    # Computed properties
    entry_points: List[str] = field(default_factory=list)  # Nodes with no callers
    orphaned_nodes: List[str] = field(default_factory=list)  # Unreachable nodes

    # Statistics
    total_calls: int = 0
    max_depth: int = 0

    def add_node(self, node: GraphNode):
        """Add a node to the graph"""
        self.nodes[node.id] = node

    def add_edge(self, edge: GraphEdge):
        """Add an edge to the graph"""
        self.edges.append(edge)
        self.total_calls += edge.call_count

    def get_node(self, node_id: str) -> Optional[GraphNode]:
        """Get a node by ID"""
        return self.nodes.get(node_id)

    def get_callers(self, node_id: str) -> List[str]:
        """Get all nodes that call this node"""
        return [edge.source for edge in self.edges if edge.target == node_id]

    def get_callees(self, node_id: str) -> List[str]:
        """Get all nodes that this node calls"""
        return [edge.target for edge in self.edges if edge.source == node_id]

    def __str__(self) -> str:
        return f"CallGraph(nodes={len(self.nodes)}, edges={len(self.edges)}, calls={self.total_calls})"


@dataclass
class ImpactAnalysis:
    """
    Result of impact analysis for a potential change

    Shows what would be affected if a node is modified
    """

    target_node: str
    target_name: str

    # Direct and transitive impact
    directly_affected: List[str] = field(default_factory=list)  # Direct callers
    transitively_affected: List[str] = field(default_factory=list)  # Indirect callers
    total_impact: int = 0

    # Risk assessment
    risk_score: float = 0.0  # 0-100
    risk_level: str = "LOW"  # LOW, MEDIUM, HIGH, CRITICAL

    # Recommendations
    warnings: List[str] = field(default_factory=list)
    recommendations: List[str] = field(default_factory=list)

    def __str__(self) -> str:
        return (
            f"ImpactAnalysis({self.target_name}): "
            f"{self.total_impact} affected nodes, "
            f"risk={self.risk_level}"
        )


@dataclass
class CallGraphStats:
    """
    Statistics about a call graph
    """

    total_nodes: int = 0
    total_edges: int = 0
    total_programs: int = 0
    total_procedures: int = 0
    total_sections: int = 0

    entry_points: int = 0
    orphaned_nodes: int = 0

    max_fan_in: int = 0
    max_fan_out: int = 0
    max_depth: int = 0

    avg_fan_in: float = 0.0
    avg_fan_out: float = 0.0

    cyclomatic_complexity: int = 0

    def __str__(self) -> str:
        return (
            f"CallGraphStats("
            f"nodes={self.total_nodes}, "
            f"edges={self.total_edges}, "
            f"entry_points={self.entry_points})"
        )
