"""
Tests for DisharmonyCalculator
"""

import pytest
import math
from cobol_harmonizer.semantic.disharmony import (
    DisharmonyCalculator,
    DisharmonyLevel
)


class TestDisharmonyCalculator:
    """Test suite for DisharmonyCalculator"""

    def setup_method(self):
        """Set up test fixtures"""
        self.calculator = DisharmonyCalculator()

    def test_calculate_identical_coords(self):
        """Test that identical coordinates have zero disharmony"""
        intent = (0.5, 0.5, 0.0, 0.0)
        execution = (0.5, 0.5, 0.0, 0.0)

        score = self.calculator.calculate(intent, execution)
        assert score == 0.0

    def test_calculate_opposite_coords(self):
        """Test calculation with opposite coordinates"""
        # Intent: Pure Wisdom
        intent = (0.0, 0.0, 0.0, 1.0)
        # Execution: Pure Power
        execution = (0.0, 0.0, 1.0, 0.0)

        score = self.calculator.calculate(intent, execution)

        # Should be sqrt(1^2 + 1^2) = sqrt(2) ≈ 1.414
        expected = math.sqrt(2)
        assert abs(score - expected) < 0.001

    def test_classify_harmonious(self):
        """Test classification of harmonious code"""
        score = 0.2
        level = self.calculator.classify(score)
        assert level == DisharmonyLevel.HARMONIOUS

    def test_classify_minor_drift(self):
        """Test classification of minor drift"""
        score = 0.4
        level = self.calculator.classify(score)
        assert level == DisharmonyLevel.MINOR_DRIFT

    def test_classify_concerning(self):
        """Test classification of concerning disharmony"""
        score = 0.6
        level = self.calculator.classify(score)
        assert level == DisharmonyLevel.CONCERNING

    def test_classify_significant(self):
        """Test classification of significant disharmony"""
        score = 1.0
        level = self.calculator.classify(score)
        assert level == DisharmonyLevel.SIGNIFICANT

    def test_classify_critical(self):
        """Test classification of critical disharmony"""
        score = 1.5
        level = self.calculator.classify(score)
        assert level == DisharmonyLevel.CRITICAL

    def test_get_trajectory(self):
        """Test trajectory calculation"""
        intent = (0.2, 0.3, 0.1, 0.4)
        execution = (0.1, 0.5, 0.3, 0.1)

        trajectory = self.calculator.get_trajectory(intent, execution)

        assert trajectory['love_delta'] == -0.1
        assert trajectory['justice_delta'] == 0.2
        assert trajectory['power_delta'] == 0.2
        assert trajectory['wisdom_delta'] == -0.3

    def test_get_dominant_shift(self):
        """Test dominant shift identification"""
        # Intent: Wisdom-dominant
        intent = (0.1, 0.1, 0.1, 0.7)
        # Execution: Power-dominant
        execution = (0.1, 0.1, 0.7, 0.1)

        from_dim, to_dim = self.calculator.get_dominant_shift(intent, execution)

        assert from_dim == 'Wisdom'
        assert to_dim == 'Power'

    def test_explain_harmonious(self):
        """Test explanation for harmonious code"""
        intent = (0.1, 0.1, 0.1, 0.7)
        execution = (0.1, 0.1, 0.1, 0.6)
        score = 0.1

        explanation = self.calculator.explain_disharmony(intent, execution, score)

        assert 'accurate' in explanation.lower() or 'aligned' in explanation.lower()

    def test_explain_critical(self):
        """Test explanation for critical disharmony"""
        intent = (0.0, 0.0, 0.0, 1.0)  # Wisdom
        execution = (0.0, 0.0, 1.0, 0.0)  # Power
        score = 1.414

        explanation = self.calculator.explain_disharmony(intent, execution, score)

        assert 'critical' in explanation.lower() or 'severe' in explanation.lower()

    def test_calculate_detailed_analysis(self):
        """Test detailed analysis generation"""
        intent = (0.1, 0.1, 0.0, 0.8)  # Wisdom-dominant (GET)
        execution = (0.0, 0.2, 0.7, 0.1)  # Power-dominant (DELETE)

        analysis = self.calculator.calculate_detailed_analysis(
            'GET-CUSTOMER-RECORD',
            intent,
            execution
        )

        # Check required fields
        assert 'procedure_name' in analysis
        assert 'disharmony_score' in analysis
        assert 'severity_level' in analysis
        assert 'intent_coords' in analysis
        assert 'execution_coords' in analysis
        assert 'trajectory' in analysis
        assert 'dominant_shift' in analysis
        assert 'explanation' in analysis
        assert 'is_harmonious' in analysis
        assert 'requires_action' in analysis

        # Check values
        assert analysis['procedure_name'] == 'GET-CUSTOMER-RECORD'
        assert analysis['disharmony_score'] > 0.5
        assert analysis['is_harmonious'] == False
        assert analysis['dominant_shift']['from'] == 'Wisdom'
        assert analysis['dominant_shift']['to'] == 'Power'

    def test_real_world_scenario_harmonious(self):
        """Test real-world harmonious example"""
        # GET-CUSTOMER-RECORD that actually reads
        intent = (0.1, 0.1, 0.0, 0.8)
        execution = (0.1, 0.2, 0.0, 0.7)  # Mostly READ operations

        score = self.calculator.calculate(intent, execution)
        level = self.calculator.classify(score)

        assert level == DisharmonyLevel.HARMONIOUS
        assert score < 0.3

    def test_real_world_scenario_critical(self):
        """Test real-world critical disharmony example"""
        # GET-CUSTOMER-BALANCE that deletes!
        intent = (0.1, 0.1, 0.0, 0.8)  # GET = Wisdom
        execution = (0.0, 0.2, 0.7, 0.1)  # DELETE = Power

        score = self.calculator.calculate(intent, execution)
        level = self.calculator.classify(score)

        assert level in [DisharmonyLevel.SIGNIFICANT, DisharmonyLevel.CRITICAL]
        assert score > 0.8

    def test_trajectory_detects_dimension_shift(self):
        """Test that trajectory correctly identifies dimension shifts"""
        # Validator that modifies state
        intent = (0.0, 0.8, 0.1, 0.1)  # VALIDATE = Justice
        execution = (0.0, 0.4, 0.5, 0.1)  # Mixed Justice + Power

        trajectory = self.calculator.get_trajectory(intent, execution)

        # Justice should decrease
        assert trajectory['justice_delta'] < 0

        # Power should increase
        assert trajectory['power_delta'] > 0

    def test_requires_action_flag(self):
        """Test requires_action flag is set correctly"""
        # Critical disharmony should require action
        intent = (0.0, 0.0, 0.0, 1.0)
        execution = (0.0, 0.0, 1.0, 0.0)

        analysis = self.calculator.calculate_detailed_analysis(
            'TEST-PROC',
            intent,
            execution
        )

        assert analysis['requires_action'] == True

        # Harmonious code should not require action
        intent2 = (0.1, 0.1, 0.1, 0.7)
        execution2 = (0.1, 0.1, 0.1, 0.6)

        analysis2 = self.calculator.calculate_detailed_analysis(
            'TEST-PROC2',
            intent2,
            execution2
        )

        assert analysis2['requires_action'] == False

    def test_coordinates_rounding(self):
        """Test that coordinates are properly rounded in analysis"""
        intent = (0.123456, 0.234567, 0.345678, 0.456789)
        execution = (0.111111, 0.222222, 0.333333, 0.444444)

        analysis = self.calculator.calculate_detailed_analysis(
            'TEST',
            intent,
            execution
        )

        # Check rounding to 3 decimal places
        assert isinstance(analysis['intent_coords']['love'], float)
        assert len(str(analysis['intent_coords']['love']).split('.')[-1]) <= 3
