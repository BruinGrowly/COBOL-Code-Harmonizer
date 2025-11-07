"""
Tests for VerbMapper
"""

import pytest
from cobol_harmonizer.semantic.verb_mapper import VerbMapper


class TestVerbMapper:
    """Test suite for VerbMapper"""

    def setup_method(self):
        """Set up test fixtures"""
        self.mapper = VerbMapper()

    def test_map_wisdom_verb(self):
        """Test mapping wisdom-dominant verb"""
        coords = self.mapper.map_verb('READ')
        l, j, p, w = coords

        # READ should be wisdom-dominant
        assert w > 0.5, "READ should be wisdom-dominant"
        assert w > p, "Wisdom should be higher than power"
        assert w > l, "Wisdom should be higher than love"

    def test_map_power_verb(self):
        """Test mapping power-dominant verb"""
        coords = self.mapper.map_verb('DELETE')
        l, j, p, w = coords

        # DELETE should be power-dominant
        assert p > 0.7, "DELETE should be power-dominant"
        assert p > w, "Power should be higher than wisdom"
        assert p > j, "Power should be higher than justice"

    def test_map_justice_verb(self):
        """Test mapping justice-dominant verb"""
        coords = self.mapper.map_verb('IF')
        l, j, p, w = coords

        # IF should be justice-dominant
        assert j > 0.7, "IF should be justice-dominant"
        assert j > p, "Justice should be higher than power"
        assert j > w, "Justice should be higher than wisdom"

    def test_map_love_verb(self):
        """Test mapping love-dominant verb"""
        coords = self.mapper.map_verb('CALL')
        l, j, p, w = coords

        # CALL should be love-dominant
        assert l > 0.4, "CALL should be love-dominant"
        assert l > j, "Love should be higher than justice"

    def test_map_unknown_verb(self):
        """Test mapping unknown verb returns default"""
        coords = self.mapper.map_verb('UNKNOWN-VERB')

        # Should return balanced default
        assert coords == (0.25, 0.25, 0.25, 0.25)

    def test_case_insensitive(self):
        """Test verb mapping is case-insensitive"""
        upper = self.mapper.map_verb('READ')
        lower = self.mapper.map_verb('read')
        mixed = self.mapper.map_verb('ReAd')

        assert upper == lower == mixed

    def test_map_intent_keyword(self):
        """Test mapping intent keywords"""
        coords = self.mapper.map_intent_keyword('GET')
        l, j, p, w = coords

        # GET should indicate wisdom intent
        assert w > 0.7, "GET should indicate wisdom intent"

    def test_map_intent_keyword_validate(self):
        """Test VALIDATE keyword"""
        coords = self.mapper.map_intent_keyword('VALIDATE')
        l, j, p, w = coords

        # VALIDATE should indicate justice intent
        assert j > 0.7, "VALIDATE should indicate justice intent"

    def test_is_vague_keyword(self):
        """Test vague keyword detection"""
        assert self.mapper.is_vague_keyword('PROCESS')
        assert self.mapper.is_vague_keyword('HANDLE')
        assert self.mapper.is_vague_keyword('MANAGE')
        assert not self.mapper.is_vague_keyword('GET')
        assert not self.mapper.is_vague_keyword('DELETE')

    def test_get_dominant_dimension(self):
        """Test dominant dimension identification"""
        # Wisdom-dominant
        wisdom_coords = (0.1, 0.1, 0.1, 0.7)
        assert self.mapper.get_dominant_dimension(wisdom_coords) == 'Wisdom'

        # Power-dominant
        power_coords = (0.1, 0.1, 0.7, 0.1)
        assert self.mapper.get_dominant_dimension(power_coords) == 'Power'

        # Justice-dominant
        justice_coords = (0.1, 0.7, 0.1, 0.1)
        assert self.mapper.get_dominant_dimension(justice_coords) == 'Justice'

        # Love-dominant
        love_coords = (0.7, 0.1, 0.1, 0.1)
        assert self.mapper.get_dominant_dimension(love_coords) == 'Love'

    def test_describe_semantics(self):
        """Test semantic description generation"""
        wisdom_coords = (0.1, 0.1, 0.1, 0.7)
        description = self.mapper.describe_semantics(wisdom_coords)

        assert 'Wisdom' in description
        assert 'information' in description or 'knowledge' in description

    def test_multiple_verbs_coverage(self):
        """Test that major COBOL verbs are covered"""
        important_verbs = [
            'READ', 'WRITE', 'REWRITE', 'DELETE',
            'MOVE', 'COMPUTE', 'ADD', 'SUBTRACT',
            'IF', 'EVALUATE', 'PERFORM', 'CALL',
            'OPEN', 'CLOSE', 'ACCEPT', 'DISPLAY'
        ]

        for verb in important_verbs:
            coords = self.mapper.map_verb(verb)
            # Should not return default for these verbs
            assert coords != (0.25, 0.25, 0.25, 0.25), \
                f"Verb {verb} should have specific mapping"

    def test_coordinates_sum_reasonable(self):
        """Test that coordinates are reasonable values"""
        test_verbs = ['READ', 'WRITE', 'IF', 'CALL', 'COMPUTE']

        for verb in test_verbs:
            coords = self.mapper.map_verb(verb)
            l, j, p, w = coords

            # All coordinates should be between 0 and 1
            assert 0 <= l <= 1, f"{verb}: Love coordinate out of range"
            assert 0 <= j <= 1, f"{verb}: Justice coordinate out of range"
            assert 0 <= p <= 1, f"{verb}: Power coordinate out of range"
            assert 0 <= w <= 1, f"{verb}: Wisdom coordinate out of range"

            # Sum should be close to 1.0 (normalized)
            total = l + j + p + w
            assert 0.8 <= total <= 1.2, f"{verb}: Coordinates sum is {total}, expected ~1.0"
