"""
Intent Extractor

Extracts intended semantics from COBOL procedure names.
"""

import re
from typing import Tuple, List
from cobol_harmonizer.semantic.verb_mapper import VerbMapper, LJPWCoords


class IntentExtractor:
    """Extract semantic intent from procedure names"""

    def __init__(self):
        self.verb_mapper = VerbMapper()

    def tokenize_name(self, name: str) -> List[str]:
        """
        Tokenize procedure name into keywords.

        Args:
            name: Procedure name (e.g., 'GET-CUSTOMER-RECORD')

        Returns:
            List of tokens
        """
        # Split by hyphens and underscores
        tokens = re.split(r"[-_]", name)
        # Filter out empty tokens
        return [t.strip() for t in tokens if t.strip()]

    def extract_intent(self, name: str) -> LJPWCoords:
        """
        Extract intended LJPW semantics from procedure name.

        Strategy:
        1. Tokenize name
        2. Map each keyword to intent coordinates
        3. Calculate weighted average (first token has more weight)

        Args:
            name: Procedure name

        Returns:
            LJPW coordinates representing intent
        """
        tokens = self.tokenize_name(name.upper())

        if not tokens:
            # Default balanced coordinates for unnamed procedures
            return (0.25, 0.25, 0.25, 0.25)

        # Collect coordinates for recognized keywords
        keyword_coords = []
        weights = []

        for idx, token in enumerate(tokens):
            # First token is most important (GET in GET-CUSTOMER-RECORD)
            # Weight decreases for subsequent tokens
            weight = 1.0 if idx == 0 else 0.5 if idx == 1 else 0.3

            # Try to map token
            coords = self.verb_mapper.map_intent_keyword(token)

            # If it's not default coords, it's a recognized keyword
            if coords != self.verb_mapper.DEFAULT_COORDS:
                keyword_coords.append(coords)
                weights.append(weight)

        if not keyword_coords:
            # No recognized keywords, return neutral
            return (0.25, 0.25, 0.25, 0.25)

        # Calculate weighted average
        total_weight = sum(weights)
        l_avg = sum(c[0] * w for c, w in zip(keyword_coords, weights)) / total_weight
        j_avg = sum(c[1] * w for c, w in zip(keyword_coords, weights)) / total_weight
        p_avg = sum(c[2] * w for c, w in zip(keyword_coords, weights)) / total_weight
        w_avg = sum(c[3] * w for c, w in zip(keyword_coords, weights)) / total_weight

        return (l_avg, j_avg, p_avg, w_avg)

    def is_vague_name(self, name: str) -> bool:
        """
        Check if procedure name is vague/ambiguous.

        Args:
            name: Procedure name

        Returns:
            True if name is vague
        """
        tokens = self.tokenize_name(name.upper())
        if not tokens:
            return True

        first_token = tokens[0]
        return self.verb_mapper.is_vague_keyword(first_token)

    def get_dominant_intent(self, name: str) -> str:
        """
        Get dominant semantic intent from procedure name.

        Args:
            name: Procedure name

        Returns:
            Dominant dimension name
        """
        coords = self.extract_intent(name)
        return self.verb_mapper.get_dominant_dimension(coords)

    def suggest_better_names(
        self, execution_coords: LJPWCoords, original_name: str
    ) -> List[Tuple[str, float]]:
        """
        Suggest better procedure names based on actual execution semantics.

        Args:
            execution_coords: Actual execution LJPW coordinates
            original_name: Original procedure name

        Returns:
            List of (suggested_name, match_score) tuples
        """
        # Get dominant dimension from execution
        dominant = self.verb_mapper.get_dominant_dimension(execution_coords)

        # Extract domain terms from original name (non-keyword tokens)
        tokens = self.tokenize_name(original_name.upper())
        domain_terms = []
        for token in tokens:
            # If not a recognized keyword, it's a domain term
            if self.verb_mapper.map_intent_keyword(token) == self.verb_mapper.DEFAULT_COORDS:
                domain_terms.append(token)

        # Suggest verbs based on dominant dimension
        suggestions = []

        dimension_verbs = {
            "Love": ["LINK", "CONNECT", "MERGE", "COMBINE", "JOIN"],
            "Justice": ["VALIDATE", "VERIFY", "CHECK", "TEST", "AUDIT"],
            "Power": ["UPDATE", "DELETE", "CREATE", "SET", "INITIALIZE"],
            "Wisdom": ["GET", "FETCH", "READ", "CALCULATE", "DISPLAY"],
        }

        verbs = dimension_verbs.get(dominant, ["PROCESS"])

        for verb in verbs:
            # Reconstruct name with better verb
            domain_part = "-".join(domain_terms) if domain_terms else "RECORD"
            new_name = f"{verb}-{domain_part}"

            # Calculate match score (how well does this verb match execution?)
            verb_coords = self.verb_mapper.map_intent_keyword(verb)
            match_score = self._calculate_similarity(verb_coords, execution_coords)

            suggestions.append((new_name, match_score))

        # Sort by match score (descending)
        suggestions.sort(key=lambda x: x[1], reverse=True)

        return suggestions[:5]  # Top 5 suggestions

    def _calculate_similarity(self, coords1: LJPWCoords, coords2: LJPWCoords) -> float:
        """
        Calculate similarity between two coordinate sets (0 to 1).

        Args:
            coords1: First coordinates
            coords2: Second coordinates

        Returns:
            Similarity score (1.0 = identical, 0.0 = opposite)
        """
        import math

        # Euclidean distance
        distance = math.sqrt(sum((c1 - c2) ** 2 for c1, c2 in zip(coords1, coords2)))

        # Convert distance to similarity (max distance is ~2.0 in normalized space)
        similarity = 1.0 - (distance / 2.0)

        return max(0.0, similarity)
