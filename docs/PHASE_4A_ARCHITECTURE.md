# Phase 4A: Enterprise Essentials - Architecture Design

**Version:** 1.0
**Date:** 2025-11-07
**Status:** Design Phase

---

## Overview

Phase 4A adds critical enterprise features that make the COBOL Code Harmonizer ready for production use at scale in mainframe environments.

### Goals

1. ✅ **Copybook Resolution** - Handle COPY statements and inline copybook content
2. ✅ **Call Graph Analysis** - Track program dependencies and call chains
3. ✅ **Batch Performance** - Analyze large codebases efficiently
4. ✅ **IDE Integration Foundation** - Enable real-time analysis in editors

---

## 1. Copybook Resolution System

### 1.1 Problem Statement

**Current State:**
```cobol
       WORKING-STORAGE SECTION.
       COPY CUSTOMER-RECORD.    ← Parser can't see inside!

       PROCEDURE DIVISION.
       VALIDATE-CUSTOMER.
           IF CUST-ID = SPACES   ← CUST-ID undefined (from copybook)
```

**What We Need:**
- Find all COPY statements
- Locate copybook files in search paths
- Parse and inline copybook content
- Handle nested COPY statements
- Support REPLACING clause
- Cache for performance

### 1.2 Architecture

```
┌─────────────────────────────────────────────────┐
│           CopybookResolver                      │
├─────────────────────────────────────────────────┤
│                                                 │
│  ┌──────────────────────────────────────────┐  │
│  │  CopybookFinder                          │  │
│  │  - Search multiple paths                 │  │
│  │  - Handle standard naming conventions    │  │
│  │  - Support wildcards                     │  │
│  └──────────────────────────────────────────┘  │
│                   ↓                             │
│  ┌──────────────────────────────────────────┐  │
│  │  CopybookCache                           │  │
│  │  - In-memory cache (LRU)                 │  │
│  │  - Disk cache (persistent)               │  │
│  │  - Invalidation on file change           │  │
│  └──────────────────────────────────────────┘  │
│                   ↓                             │
│  ┌──────────────────────────────────────────┐  │
│  │  CopybookParser                          │  │
│  │  - Parse copybook content                │  │
│  │  - Handle REPLACING clause               │  │
│  │  - Detect circular dependencies          │  │
│  └──────────────────────────────────────────┘  │
│                   ↓                             │
│  ┌──────────────────────────────────────────┐  │
│  │  CopybookInliner                         │  │
│  │  - Inline content at COPY location       │  │
│  │  - Preserve line numbers for errors      │  │
│  │  - Generate source map                   │  │
│  └──────────────────────────────────────────┘  │
│                                                 │
└─────────────────────────────────────────────────┘
```

### 1.3 Data Structures

```python
@dataclass
class CopyStatement:
    """Represents a COPY statement in source code"""
    copybook_name: str
    line_number: int
    replacing_clauses: List[ReplacingClause]
    resolved_path: Optional[str] = None
    content: Optional[str] = None

@dataclass
class ReplacingClause:
    """COPY ... REPLACING clause"""
    original: str
    replacement: str

@dataclass
class Copybook:
    """Resolved copybook information"""
    name: str
    path: str
    content: str
    nested_copies: List[CopyStatement]
    hash: str  # For cache invalidation
    source_map: SourceMap  # Original line → resolved line mapping

@dataclass
class SourceMap:
    """Maps resolved source back to original files"""
    mappings: Dict[int, SourceLocation]

@dataclass
class SourceLocation:
    """Location in original source"""
    file_path: str
    line_number: int
    is_copybook: bool
```

### 1.4 Configuration

```python
@dataclass
class CopybookConfig:
    """Configuration for copybook resolution"""

    # Search paths (in order of priority)
    search_paths: List[str] = field(default_factory=lambda: [
        './copybooks',
        './copy',
        '../copybooks',
        '/copybook/lib',  # Common mainframe location
    ])

    # File extensions to try
    extensions: List[str] = field(default_factory=lambda: [
        '.cpy',
        '.CPY',
        '.cbl',
        '.CBL',
        '',  # No extension
    ])

    # Cache settings
    enable_cache: bool = True
    cache_dir: str = '.harmonizer-cache/copybooks'
    max_cache_size_mb: int = 100

    # Resolution settings
    max_depth: int = 10  # Prevent infinite recursion
    preserve_comments: bool = True
    resolve_nested: bool = True
```

### 1.5 API Design

```python
class CopybookResolver:
    """Main copybook resolution system"""

    def __init__(self, config: CopybookConfig):
        self.config = config
        self.finder = CopybookFinder(config)
        self.cache = CopybookCache(config)
        self.parser = COBOLParser()

    def resolve_program(self,
                       source_path: str,
                       source_content: str) -> ResolvedSource:
        """
        Resolve all copybooks in a COBOL program

        Returns:
            ResolvedSource with all copybooks inlined
        """
        # 1. Find all COPY statements
        copy_statements = self.find_copy_statements(source_content)

        # 2. Resolve each copybook
        for copy_stmt in copy_statements:
            copybook = self.resolve_copybook(copy_stmt)
            copy_stmt.content = copybook.content

        # 3. Inline copybooks
        resolved = self.inline_copybooks(source_content, copy_statements)

        return resolved

    def find_copy_statements(self, source: str) -> List[CopyStatement]:
        """Find all COPY statements in source"""
        pass

    def resolve_copybook(self,
                        copy_stmt: CopyStatement,
                        depth: int = 0) -> Copybook:
        """Resolve a single copybook (recursive for nested)"""

        # Check cache first
        if self.config.enable_cache:
            cached = self.cache.get(copy_stmt.copybook_name)
            if cached:
                return cached

        # Find copybook file
        path = self.finder.find(copy_stmt.copybook_name)
        if not path:
            raise CopybookNotFoundError(copy_stmt.copybook_name)

        # Read and parse
        content = self.read_copybook(path)

        # Handle REPLACING clause
        if copy_stmt.replacing_clauses:
            content = self.apply_replacing(content, copy_stmt.replacing_clauses)

        # Resolve nested COPY statements
        nested_copies = []
        if self.config.resolve_nested and depth < self.config.max_depth:
            nested_copies = self.find_copy_statements(content)
            for nested in nested_copies:
                nested_copybook = self.resolve_copybook(nested, depth + 1)
                # Inline nested copybook
                content = content.replace(
                    f"COPY {nested.copybook_name}",
                    nested_copybook.content
                )

        # Create copybook object
        copybook = Copybook(
            name=copy_stmt.copybook_name,
            path=path,
            content=content,
            nested_copies=nested_copies,
            hash=self.compute_hash(content),
            source_map=self.create_source_map(path, content)
        )

        # Cache it
        if self.config.enable_cache:
            self.cache.put(copy_stmt.copybook_name, copybook)

        return copybook
```

---

## 2. Call Graph Analysis

### 2.1 Problem Statement

**What We Need:**
- Track all CALL statements (inter-program calls)
- Track all PERFORM statements (intra-program calls)
- Build dependency graph
- Identify orphaned procedures
- Calculate ripple impact
- Detect circular dependencies

### 2.2 Architecture

```
┌─────────────────────────────────────────────────┐
│           CallGraphAnalyzer                     │
├─────────────────────────────────────────────────┤
│                                                 │
│  ┌──────────────────────────────────────────┐  │
│  │  CallExtractor                           │  │
│  │  - Find CALL statements                  │  │
│  │  - Find PERFORM statements               │  │
│  │  - Extract program names                 │  │
│  └──────────────────────────────────────────┘  │
│                   ↓                             │
│  ┌──────────────────────────────────────────┐  │
│  │  GraphBuilder                            │  │
│  │  - Build directed graph                  │  │
│  │  - Track caller → callee relationships   │  │
│  │  - Handle dynamic calls                  │  │
│  └──────────────────────────────────────────┘  │
│                   ↓                             │
│  ┌──────────────────────────────────────────┐  │
│  │  GraphAnalyzer                           │  │
│  │  - Find strongly connected components    │  │
│  │  - Calculate metrics (fan-in/fan-out)    │  │
│  │  - Identify entry points                 │  │
│  │  - Find orphaned code                    │  │
│  └──────────────────────────────────────────┘  │
│                   ↓                             │
│  ┌──────────────────────────────────────────┐  │
│  │  ImpactAnalyzer                          │  │
│  │  - Calculate ripple effects              │  │
│  │  - Find all dependencies                 │  │
│  │  - Risk scoring                          │  │
│  └──────────────────────────────────────────┘  │
│                                                 │
└─────────────────────────────────────────────────┘
```

### 2.3 Data Structures

```python
@dataclass
class CallSite:
    """A single call location"""
    caller: str  # Procedure or program name
    callee: str  # What's being called
    call_type: CallType  # CALL, PERFORM, etc.
    line_number: int
    is_dynamic: bool  # CALL using variable

class CallType(Enum):
    PROGRAM_CALL = "CALL"  # Inter-program
    PROCEDURE_PERFORM = "PERFORM"  # Intra-program
    SECTION_PERFORM = "PERFORM-SECTION"

@dataclass
class CallGraph:
    """Complete call graph for codebase"""
    nodes: Dict[str, GraphNode]  # node_id → node
    edges: List[GraphEdge]
    entry_points: List[str]
    orphaned_nodes: List[str]

@dataclass
class GraphNode:
    """Node in call graph"""
    id: str
    name: str
    node_type: NodeType  # PROGRAM, PROCEDURE, SECTION
    file_path: Optional[str]
    metrics: NodeMetrics

class NodeType(Enum):
    PROGRAM = "program"
    PROCEDURE = "procedure"
    SECTION = "section"

@dataclass
class NodeMetrics:
    """Metrics for a graph node"""
    fan_in: int  # How many call this
    fan_out: int  # How many this calls
    depth: int  # Distance from entry point
    complexity: int  # Cyclomatic complexity

@dataclass
class GraphEdge:
    """Edge in call graph"""
    source: str  # Caller ID
    target: str  # Callee ID
    call_type: CallType
    call_count: int  # How many times

@dataclass
class ImpactAnalysis:
    """Result of impact analysis for a change"""
    target_node: str
    directly_affected: List[str]  # Direct callers
    transitively_affected: List[str]  # Indirect callers
    total_impact: int
    risk_score: float  # 0-100
```

### 2.4 API Design

```python
class CallGraphAnalyzer:
    """Analyzes program call structure"""

    def __init__(self):
        self.extractor = CallExtractor()
        self.builder = GraphBuilder()

    def analyze_program(self, program: COBOLProgram) -> ProgramCallGraph:
        """Analyze calls within a single program"""

        # Extract all call sites
        calls = self.extractor.extract_calls(program)

        # Build intra-program graph
        graph = self.builder.build_intra_program_graph(
            program,
            calls
        )

        return graph

    def analyze_codebase(self, programs: List[COBOLProgram]) -> CallGraph:
        """Analyze calls across entire codebase"""

        # Extract all calls from all programs
        all_calls = []
        for program in programs:
            calls = self.extractor.extract_calls(program)
            all_calls.extend(calls)

        # Build complete call graph
        graph = self.builder.build_complete_graph(
            programs,
            all_calls
        )

        # Analyze graph
        self.identify_entry_points(graph)
        self.find_orphaned_nodes(graph)
        self.calculate_metrics(graph)

        return graph

    def analyze_impact(self,
                      graph: CallGraph,
                      target_node: str) -> ImpactAnalysis:
        """Analyze impact of changing a node"""

        # Find all nodes that call this (directly or indirectly)
        affected = self.find_all_callers(graph, target_node)

        # Calculate risk score
        risk = self.calculate_change_risk(graph, target_node, affected)

        return ImpactAnalysis(
            target_node=target_node,
            directly_affected=[n for n in affected if self.is_direct_caller(graph, n, target_node)],
            transitively_affected=[n for n in affected if not self.is_direct_caller(graph, n, target_node)],
            total_impact=len(affected),
            risk_score=risk
        )
```

---

## 3. Batch Performance Optimization

### 3.1 Performance Targets

| Metric | Current | Target | Strategy |
|--------|---------|--------|----------|
| **Parse Speed** | ~500 LOC/sec | 5,000 LOC/sec | Multiprocessing |
| **Memory Usage** | ~50 MB/file | ~10 MB/file | Streaming |
| **Cache Hit Rate** | 0% | >80% | Smart caching |
| **Startup Time** | ~2 sec | <0.5 sec | Lazy loading |

### 3.2 Architecture

```
┌─────────────────────────────────────────────────┐
│           BatchAnalyzer                         │
├─────────────────────────────────────────────────┤
│                                                 │
│  ┌──────────────────────────────────────────┐  │
│  │  FileDiscovery                           │  │
│  │  - Walk directory tree                   │  │
│  │  - Filter by extension                   │  │
│  │  - Skip ignored patterns                 │  │
│  └──────────────────────────────────────────┘  │
│                   ↓                             │
│  ┌──────────────────────────────────────────┐  │
│  │  IncrementalAnalyzer                     │  │
│  │  - Track file hashes                     │  │
│  │  - Only analyze changed files            │  │
│  │  - Reuse cached results                  │  │
│  └──────────────────────────────────────────┘  │
│                   ↓                             │
│  ┌──────────────────────────────────────────┐  │
│  │  ParallelProcessor                       │  │
│  │  - Process pool (N workers)              │  │
│  │  - Work queue                            │  │
│  │  - Result aggregation                    │  │
│  └──────────────────────────────────────────┘  │
│                   ↓                             │
│  ┌──────────────────────────────────────────┐  │
│  │  ProgressTracker                         │  │
│  │  - Real-time progress updates            │  │
│  │  - ETA calculation                       │  │
│  │  - Error tracking                        │  │
│  └──────────────────────────────────────────┘  │
│                                                 │
└─────────────────────────────────────────────────┘
```

### 3.3 Implementation Strategy

```python
class BatchAnalyzer:
    """High-performance batch analysis"""

    def __init__(self, num_workers: int = None):
        self.num_workers = num_workers or os.cpu_count()
        self.cache = AnalysisCache()

    def analyze_directory(self,
                         path: str,
                         config: AnalysisConfig) -> BatchResults:
        """Analyze all COBOL files in directory"""

        # 1. Discover files
        files = self.discover_files(path, config.patterns)

        # 2. Check cache (skip unchanged files)
        if config.incremental:
            files = self.filter_changed_files(files)

        # 3. Parallel processing
        with ProcessPoolExecutor(max_workers=self.num_workers) as executor:
            futures = {
                executor.submit(self.analyze_file, f): f
                for f in files
            }

            results = []
            for future in as_completed(futures):
                try:
                    result = future.result()
                    results.append(result)
                    self.update_progress(len(results), len(files))
                except Exception as e:
                    logger.error(f"Failed to analyze {futures[future]}: {e}")

        # 4. Aggregate results
        return self.aggregate_results(results)
```

---

## 4. Foundation for IDE Integration

### 4.1 Language Server Protocol (LSP)

**Goal:** Enable real-time analysis in VS Code, Eclipse, etc.

```
┌─────────────────────────────────────────────────┐
│     COBOL Code Harmonizer LSP Server            │
├─────────────────────────────────────────────────┤
│                                                 │
│  ┌──────────────────────────────────────────┐  │
│  │  DocumentManager                         │  │
│  │  - Track open documents                  │  │
│  │  - Handle edits incrementally            │  │
│  │  - Maintain parse trees                  │  │
│  └──────────────────────────────────────────┘  │
│                   ↓                             │
│  ┌──────────────────────────────────────────┐  │
│  │  DiagnosticsProvider                     │  │
│  │  - Semantic analysis on change           │  │
│  │  - Generate warnings/errors              │  │
│  │  - Update in real-time                   │  │
│  └──────────────────────────────────────────┘  │
│                   ↓                             │
│  ┌──────────────────────────────────────────┐  │
│  │  CodeActionProvider                      │  │
│  │  - Suggest fixes                         │  │
│  │  - Quick refactorings                    │  │
│  │  - Rename suggestions                    │  │
│  └──────────────────────────────────────────┘  │
│                                                 │
└─────────────────────────────────────────────────┘
```

### 4.2 VS Code Extension (Phase 4B)

For now, we'll create the foundation. Full extension in Phase 4B.

---

## Implementation Plan

### Week 1-2: Copybook Resolution
- [ ] Day 1-2: CopybookFinder + basic tests
- [ ] Day 3-4: CopybookCache implementation
- [ ] Day 5-6: CopybookResolver integration
- [ ] Day 7-8: REPLACING clause support
- [ ] Day 9-10: Comprehensive testing

### Week 3-4: Call Graph Analysis
- [ ] Day 1-3: CallExtractor (CALL + PERFORM)
- [ ] Day 4-5: GraphBuilder
- [ ] Day 6-7: Impact analysis
- [ ] Day 8-9: Visualization export (GraphViz)
- [ ] Day 10: Testing and documentation

### Week 5: Performance & Polish
- [ ] Day 1-2: Batch performance optimization
- [ ] Day 3: Integration testing
- [ ] Day 4-5: Documentation and examples

---

## Success Metrics

**Copybook Resolution:**
- ✅ Resolve 95%+ of copybooks in test codebase
- ✅ Handle nested copybooks (3+ levels deep)
- ✅ Cache hit rate >80%
- ✅ Performance: <100ms per copybook

**Call Graph:**
- ✅ Identify all CALL and PERFORM statements
- ✅ Build accurate dependency graph
- ✅ Calculate metrics for all nodes
- ✅ Export to GraphViz format

**Performance:**
- ✅ Process 1000 files in <5 minutes
- ✅ Memory usage <500 MB for large codebases
- ✅ Incremental analysis <1 second for single file changes

---

## Next Steps

1. Create implementation branch: `feature/phase-4a-enterprise-essentials`
2. Implement copybook resolution (highest priority)
3. Add comprehensive tests
4. Implement call graph analysis
5. Performance optimization
6. Documentation and examples

---

**Document Version:** 1.0
**Last Updated:** 2025-11-07
**Status:** ✅ Design Complete, Ready for Implementation
