"""
Tests for CodebaseMapper
"""

import pytest
from cobol_harmonizer.codebase_mapper import CodebaseMapper


class TestCodebaseMapper:
    """Test suite for CodebaseMapper"""

    def setup_method(self):
        """Set up test fixtures"""
        self.mapper = CodebaseMapper()

        self.sample_batch_results = {
            "statistics": {
                "total_procedures": 10,
                "harmonious_count": 5,
                "disharmonious_count": 5,
                "requires_action_count": 3,
                "files_with_issues": 2,
                "files_clean": 1,
                "severity_breakdown": {
                    "harmonious": 5,
                    "minor_drift": 2,
                    "concerning": 1,
                    "significant": 1,
                    "critical": 1
                }
            },
            "file_results": [
                {
                    "file_path": "file1.cbl",
                    "program_id": "PROG1",
                    "total_procedures": 5,
                    "results": [
                        {
                            "procedure_name": "PROC1",
                            "disharmony_score": 1.5,
                            "severity_level": "critical",
                            "is_harmonious": False,
                            "requires_action": True,
                            "intent_coords": {"love": 0.0, "justice": 0.0, "power": 0.0, "wisdom": 1.0},
                            "execution_coords": {"love": 0.0, "justice": 0.0, "power": 1.0, "wisdom": 0.0},
                            "dominant_shift": {"from": "Wisdom", "to": "Power"}
                        },
                        {
                            "procedure_name": "PROC2",
                            "disharmony_score": 0.2,
                            "severity_level": "harmonious",
                            "is_harmonious": True,
                            "requires_action": False,
                            "intent_coords": {"love": 0.1, "justice": 0.2, "power": 0.1, "wisdom": 0.6},
                            "execution_coords": {"love": 0.1, "justice": 0.2, "power": 0.1, "wisdom": 0.6},
                            "dominant_shift": {"from": "Wisdom", "to": "Wisdom"}
                        }
                    ]
                },
                {
                    "file_path": "file2.cbl",
                    "program_id": "PROG2",
                    "total_procedures": 3,
                    "results": [
                        {
                            "procedure_name": "PROC3",
                            "disharmony_score": 0.9,
                            "severity_level": "significant",
                            "is_harmonious": False,
                            "requires_action": True,
                            "intent_coords": {"love": 0.5, "justice": 0.1, "power": 0.3, "wisdom": 0.1},
                            "execution_coords": {"love": 0.1, "justice": 0.7, "power": 0.1, "wisdom": 0.1},
                            "dominant_shift": {"from": "Love", "to": "Justice"}
                        }
                    ]
                }
            ]
        }

    def test_analyze_codebase(self):
        """Test complete codebase analysis"""
        analysis = self.mapper.analyze_codebase(self.sample_batch_results)

        assert "overview" in analysis
        assert "health_metrics" in analysis
        assert "hotspots" in analysis
        assert "semantic_patterns" in analysis
        assert "recommendations" in analysis
        assert "file_rankings" in analysis
        assert "procedure_rankings" in analysis
        assert "dimension_analysis" in analysis

    def test_generate_overview(self):
        """Test overview generation"""
        overview = self.mapper._generate_overview(self.sample_batch_results)

        assert overview["total_files"] == 2
        assert overview["total_procedures"] == 10
        assert overview["harmonious_count"] == 5
        assert overview["disharmonious_count"] == 5
        assert overview["requires_action_count"] == 3

    def test_calculate_health_metrics(self):
        """Test health metrics calculation"""
        health = self.mapper._calculate_health_metrics(self.sample_batch_results)

        assert "health_score" in health
        assert "harmony_rate" in health
        assert "action_required_rate" in health
        assert "grade" in health
        assert "interpretation" in health

        # Check ranges
        assert 0 <= health["health_score"] <= 100
        assert 0 <= health["harmony_rate"] <= 100
        assert health["grade"] in ["A", "B", "C", "D", "F"]

    def test_health_score_calculation(self):
        """Test health score calculation logic"""
        # Test with good codebase
        good_results = {
            "statistics": {
                "total_procedures": 10,
                "harmonious_count": 9,
                "disharmonious_count": 1,
                "requires_action_count": 0,
                "severity_breakdown": {
                    "harmonious": 9,
                    "minor_drift": 1
                }
            },
            "file_results": []
        }

        health = self.mapper._calculate_health_metrics(good_results)
        assert health["health_score"] > 80
        assert health["grade"] in ["A", "B"]

        # Test with bad codebase
        bad_results = {
            "statistics": {
                "total_procedures": 10,
                "harmonious_count": 2,
                "disharmonious_count": 8,
                "requires_action_count": 8,
                "severity_breakdown": {
                    "harmonious": 2,
                    "critical": 8
                }
            },
            "file_results": []
        }

        health = self.mapper._calculate_health_metrics(bad_results)
        assert health["health_score"] < 50
        assert health["grade"] in ["D", "F"]

    def test_identify_hotspots(self):
        """Test hotspot identification"""
        hotspots = self.mapper._identify_hotspots(self.sample_batch_results)

        assert "worst_files" in hotspots
        assert "critical_files" in hotspots

        # Should identify file1.cbl as a hotspot (has critical issue)
        worst_files = hotspots["worst_files"]
        assert len(worst_files) > 0

        critical_files = hotspots["critical_files"]
        assert len(critical_files) > 0
        assert any(f["file_path"] == "file1.cbl" for f in critical_files)

    def test_analyze_semantic_patterns(self):
        """Test semantic pattern analysis"""
        patterns = self.mapper._analyze_semantic_patterns(self.sample_batch_results)

        assert "common_shifts" in patterns
        assert "dimension_drifts" in patterns
        assert "most_unstable_dimension" in patterns

        # Should detect Wisdom → Power shift
        common_shifts = patterns["common_shifts"]
        assert any("Wisdom → Power" in shift[0] for shift in common_shifts)

    def test_generate_recommendations(self):
        """Test recommendation generation"""
        recommendations = self.mapper._generate_recommendations(self.sample_batch_results)

        assert isinstance(recommendations, list)
        assert len(recommendations) > 0

        # Should have critical recommendation
        assert any(r["priority"] == "CRITICAL" for r in recommendations)

        # Check recommendation structure
        for rec in recommendations:
            assert "priority" in rec
            assert "category" in rec
            assert "title" in rec
            assert "description" in rec
            assert "action" in rec

    def test_rank_files(self):
        """Test file ranking"""
        rankings = self.mapper._rank_files(self.sample_batch_results)

        assert isinstance(rankings, list)
        assert len(rankings) == 2

        # Check ranking structure
        for ranking in rankings:
            assert "file_path" in ranking
            assert "program_id" in ranking
            assert "total_procedures" in ranking
            assert "avg_disharmony" in ranking
            assert "harmonious_count" in ranking
            assert "harmony_rate" in ranking

        # Files should be sorted by harmony rate (worst first)
        if len(rankings) > 1:
            assert rankings[0]["harmony_rate"] <= rankings[1]["harmony_rate"]

    def test_rank_procedures(self):
        """Test procedure ranking"""
        rankings = self.mapper._rank_procedures(self.sample_batch_results)

        assert isinstance(rankings, list)
        assert len(rankings) >= 3

        # Should be sorted by disharmony score (descending)
        if len(rankings) > 1:
            assert rankings[0]["disharmony_score"] >= rankings[1]["disharmony_score"]

        # PROC1 should be first (highest score: 1.5)
        assert rankings[0]["procedure_name"] == "PROC1"
        assert rankings[0]["disharmony_score"] == 1.5

    def test_analyze_dimensions(self):
        """Test dimension analysis"""
        dim_analysis = self.mapper._analyze_dimensions(self.sample_batch_results)

        assert "intent_distribution" in dim_analysis
        assert "execution_distribution" in dim_analysis
        assert "dominant_intent_dimension" in dim_analysis
        assert "dominant_execution_dimension" in dim_analysis
        assert "alignment" in dim_analysis

        # Check that distributions have all dimensions
        intent_dist = dim_analysis["intent_distribution"]
        assert "Love" in intent_dist
        assert "Justice" in intent_dist
        assert "Power" in intent_dist
        assert "Wisdom" in intent_dist

    def test_generate_summary_report(self):
        """Test summary report generation"""
        analysis = self.mapper.analyze_codebase(self.sample_batch_results)
        report = self.mapper.generate_summary_report(analysis)

        assert isinstance(report, str)
        assert "COBOL CODEBASE ANALYSIS REPORT" in report
        assert "OVERVIEW" in report
        assert "CODEBASE HEALTH" in report
        assert "RECOMMENDATIONS" in report

        # Should include key metrics
        assert "Total Files" in report
        assert "Total Procedures" in report
        assert "Health Score" in report

    def test_empty_codebase(self):
        """Test analysis with empty codebase"""
        empty_results = {
            "statistics": {},
            "file_results": []
        }

        analysis = self.mapper.analyze_codebase(empty_results)

        assert analysis["status"] == "empty"

    def test_interpret_health_score(self):
        """Test health score interpretation"""
        # Excellent
        interp = self.mapper._interpret_health_score(95)
        assert "Excellent" in interp

        # Good
        interp = self.mapper._interpret_health_score(85)
        assert "Good" in interp

        # Fair
        interp = self.mapper._interpret_health_score(75)
        assert "Fair" in interp

        # Poor
        interp = self.mapper._interpret_health_score(65)
        assert "Poor" in interp

        # Critical
        interp = self.mapper._interpret_health_score(50)
        assert "Critical" in interp

    def test_severity_weights(self):
        """Test severity weight mapping"""
        assert self.mapper.severity_weights["harmonious"] == 0
        assert self.mapper.severity_weights["minor_drift"] == 1
        assert self.mapper.severity_weights["concerning"] == 2
        assert self.mapper.severity_weights["significant"] == 3
        assert self.mapper.severity_weights["critical"] == 4

    def test_dimension_drift_detection(self):
        """Test dimension drift detection"""
        patterns = self.mapper._analyze_semantic_patterns(self.sample_batch_results)

        dimension_drifts = patterns["dimension_drifts"]

        # Wisdom has one drift (Wisdom → Power)
        assert dimension_drifts["Wisdom"] >= 1

        # Love has one drift (Love → Justice)
        assert dimension_drifts["Love"] >= 1

    def test_recommendations_priority_levels(self):
        """Test that recommendations have appropriate priorities"""
        recommendations = self.mapper._generate_recommendations(self.sample_batch_results)

        priorities = {r["priority"] for r in recommendations}

        # Should have multiple priority levels
        assert len(priorities) > 0
        assert all(p in ["CRITICAL", "HIGH", "MEDIUM", "LOW"] for p in priorities)
