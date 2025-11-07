"""
Call Graph Visualizer

Exports call graphs to various formats for visualization:
- GraphViz DOT format
- JSON for web visualization
- ASCII art for terminal
"""

from typing import Optional, Set
from io import StringIO

from .models import CallGraph, GraphNode, NodeType, CallType


class CallGraphVisualizer:
    """
    Visualizes call graphs in various formats

    Supports:
    - GraphViz DOT format (for rendering with dot/graphviz)
    - JSON format (for D3.js or other web visualizations)
    - ASCII art (for terminal display)
    """

    def __init__(self, graph: CallGraph):
        """
        Initialize visualizer with a call graph

        Args:
            graph: CallGraph to visualize
        """
        self.graph = graph

    def to_dot(
        self,
        highlight_nodes: Optional[Set[str]] = None,
        show_metrics: bool = True,
        show_line_numbers: bool = False
    ) -> str:
        """
        Export to GraphViz DOT format

        Args:
            highlight_nodes: Set of node IDs to highlight
            show_metrics: Show fan-in/fan-out metrics
            show_line_numbers: Show line numbers on edges

        Returns:
            DOT format string
        """
        out = StringIO()
        highlight_nodes = highlight_nodes or set()

        # Header
        out.write("digraph CallGraph {\n")
        out.write("  rankdir=TB;\n")  # Top to bottom
        out.write("  node [shape=box, style=rounded];\n")
        out.write("  edge [fontsize=10];\n")
        out.write("\n")

        # Add nodes
        for node_id, node in self.graph.nodes.items():
            color = self._get_node_color(node, node_id in highlight_nodes)
            shape = self._get_node_shape(node)

            # Build label
            label = self._build_node_label(node, show_metrics)

            # Write node
            out.write(f'  "{node_id}" [\n')
            out.write(f'    label="{label}",\n')
            out.write(f'    shape={shape},\n')
            out.write(f'    fillcolor="{color}",\n')
            out.write('    style="filled,rounded"\n')
            out.write("  ];\n")

        out.write("\n")

        # Add edges
        for edge in self.graph.edges:
            style = self._get_edge_style(edge.call_type)
            label = ""

            if show_line_numbers and edge.line_numbers:
                lines = edge.line_numbers[:3]  # Show first 3
                label = f'L{",".join(map(str, lines))}'
                if len(edge.line_numbers) > 3:
                    label += "..."

            if edge.call_count > 1:
                if label:
                    label += f" ({edge.call_count}x)"
                else:
                    label = f"{edge.call_count}x"

            out.write(f'  "{edge.source}" -> "{edge.target}"')
            if label or style != "solid":
                out.write(" [")
                if label:
                    out.write(f'label="{label}"')
                    if style != "solid":
                        out.write(", ")
                if style != "solid":
                    out.write(f'style="{style}"')
                out.write("]")
            out.write(";\n")

        out.write("}\n")

        return out.getvalue()

    def _get_node_color(self, node: GraphNode, is_highlighted: bool) -> str:
        """Get color for node based on type and metrics"""
        if is_highlighted:
            return "yellow"

        # Color by node type
        if node.node_type == NodeType.PROGRAM:
            return "lightblue"
        elif node.node_type == NodeType.SECTION:
            return "lightgreen"
        elif node.node_type == NodeType.PARAGRAPH:
            return "lightyellow"
        else:
            return "white"

    def _get_node_shape(self, node: GraphNode) -> str:
        """Get shape for node based on type"""
        if node.node_type == NodeType.PROGRAM:
            return "box"
        elif node.node_type == NodeType.SECTION:
            return "hexagon"
        else:
            return "ellipse"

    def _build_node_label(self, node: GraphNode, show_metrics: bool) -> str:
        """Build label for node"""
        label = node.name

        if show_metrics:
            metrics = node.metrics
            label += f"\\n"
            label += f"in:{metrics.fan_in} out:{metrics.fan_out}"

            if metrics.depth > 0:
                label += f" d:{metrics.depth}"

        return label

    def _get_edge_style(self, call_type: CallType) -> str:
        """Get edge style based on call type"""
        if call_type == CallType.PROGRAM_CALL:
            return "bold"
        elif call_type == CallType.DYNAMIC_CALL:
            return "dashed"
        else:
            return "solid"

    def to_json(self) -> dict:
        """
        Export to JSON format for web visualization

        Returns:
            Dictionary in D3.js force-directed graph format
        """
        # Build nodes list
        nodes = []
        for node_id, node in self.graph.nodes.items():
            nodes.append({
                "id": node_id,
                "name": node.name,
                "type": node.node_type.value,
                "fan_in": node.metrics.fan_in,
                "fan_out": node.metrics.fan_out,
                "depth": node.metrics.depth,
                "complexity": node.metrics.complexity,
                "file": node.file_path,
                "line": node.start_line,
            })

        # Build links list
        links = []
        for edge in self.graph.edges:
            links.append({
                "source": edge.source,
                "target": edge.target,
                "type": edge.call_type.value,
                "count": edge.call_count,
                "lines": edge.line_numbers,
            })

        return {
            "nodes": nodes,
            "links": links,
            "entry_points": self.graph.entry_points,
            "orphaned_nodes": self.graph.orphaned_nodes,
            "stats": {
                "total_nodes": len(self.graph.nodes),
                "total_edges": len(self.graph.edges),
                "max_depth": self.graph.max_depth,
            }
        }

    def to_ascii(self, max_depth: int = 3, entry_point: Optional[str] = None) -> str:
        """
        Export to ASCII art (tree view)

        Args:
            max_depth: Maximum depth to show
            entry_point: Entry point to start from (or first entry point)

        Returns:
            ASCII art string
        """
        out = StringIO()

        # Choose entry point
        if entry_point is None:
            if not self.graph.entry_points:
                return "No entry points found"
            entry_point = self.graph.entry_points[0]

        # Build tree
        visited = set()

        def print_tree(node_id: str, prefix: str = "", depth: int = 0):
            if depth > max_depth or node_id in visited:
                if node_id in visited:
                    out.write(f"{prefix}↻ (circular)\n")
                return

            visited.add(node_id)

            node = self.graph.get_node(node_id)
            if not node:
                return

            # Print current node
            symbol = self._get_node_symbol(node)
            out.write(f"{prefix}{symbol} {node.name}")

            # Show metrics
            if node.metrics.fan_in > 0 or node.metrics.fan_out > 0:
                out.write(f" (in:{node.metrics.fan_in} out:{node.metrics.fan_out})")

            out.write("\n")

            # Print children
            callees = self.graph.get_callees(node_id)
            for i, callee_id in enumerate(callees):
                is_last = (i == len(callees) - 1)
                new_prefix = prefix + ("    " if is_last else "│   ")
                branch = "└── " if is_last else "├── "
                print_tree(callee_id, prefix + branch, depth + 1)

        # Print header
        out.write("Call Graph Tree\n")
        out.write("=" * 60 + "\n\n")

        # Print tree
        print_tree(entry_point)

        out.write("\n")
        out.write(f"Symbols: 📦=Program, 📑=Section, 📄=Paragraph\n")

        return out.getvalue()

    def _get_node_symbol(self, node: GraphNode) -> str:
        """Get symbol for node type"""
        if node.node_type == NodeType.PROGRAM:
            return "📦"
        elif node.node_type == NodeType.SECTION:
            return "📑"
        elif node.node_type == NodeType.PARAGRAPH:
            return "📄"
        else:
            return "•"

    def generate_summary(self) -> str:
        """
        Generate a text summary of the call graph

        Returns:
            Summary string
        """
        out = StringIO()

        out.write("Call Graph Summary\n")
        out.write("=" * 60 + "\n\n")

        # Basic stats
        out.write(f"Total nodes: {len(self.graph.nodes)}\n")
        out.write(f"Total edges: {len(self.graph.edges)}\n")
        out.write(f"Total calls: {self.graph.total_calls}\n")
        out.write(f"Max depth: {self.graph.max_depth}\n")
        out.write("\n")

        # Entry points
        out.write(f"Entry points: {len(self.graph.entry_points)}\n")
        for ep in self.graph.entry_points[:5]:
            node = self.graph.get_node(ep)
            if node:
                out.write(f"  • {node.name}\n")
        if len(self.graph.entry_points) > 5:
            out.write(f"  ... and {len(self.graph.entry_points) - 5} more\n")
        out.write("\n")

        # Orphaned nodes
        if self.graph.orphaned_nodes:
            out.write(f"Orphaned nodes (dead code): {len(self.graph.orphaned_nodes)}\n")
            for orphan_id in self.graph.orphaned_nodes[:5]:
                node = self.graph.get_node(orphan_id)
                if node:
                    out.write(f"  • {node.name}\n")
            if len(self.graph.orphaned_nodes) > 5:
                out.write(f"  ... and {len(self.graph.orphaned_nodes) - 5} more\n")
            out.write("\n")

        # Top fan-in nodes (most called)
        nodes_by_fan_in = sorted(
            self.graph.nodes.values(),
            key=lambda n: n.metrics.fan_in,
            reverse=True
        )
        out.write("Most called nodes (top 5):\n")
        for node in nodes_by_fan_in[:5]:
            if node.metrics.fan_in > 0:
                out.write(f"  • {node.name} (called {node.metrics.fan_in} times)\n")
        out.write("\n")

        # Top fan-out nodes (call most)
        nodes_by_fan_out = sorted(
            self.graph.nodes.values(),
            key=lambda n: n.metrics.fan_out,
            reverse=True
        )
        out.write("Nodes that call most (top 5):\n")
        for node in nodes_by_fan_out[:5]:
            if node.metrics.fan_out > 0:
                out.write(f"  • {node.name} (calls {node.metrics.fan_out} others)\n")

        return out.getvalue()
