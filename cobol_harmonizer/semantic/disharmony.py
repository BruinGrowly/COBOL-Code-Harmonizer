"""
Disharmony Calculator

Calculates semantic distance between intent and execution in LJPW space.
"""

import math
from typing import Tuple, Dict
from enum import Enum

# Type alias for LJPW coordinates
LJPWCoords = Tuple[float, float, float, float]


class DisharmonyLevel(Enum):
    """Classification of disharmony severity"""
    HARMONIOUS = "harmonious"          # 0.0 - 0.3
    MINOR_DRIFT = "minor_drift"        # 0.3 - 0.5
    CONCERNING = "concerning"          # 0.5 - 0.8
    SIGNIFICANT = "significant"        # 0.8 - 1.2
    CRITICAL = "critical"              # 1.2+


class DisharmonyCalculator:
    """Calculate semantic disharmony between intent and execution"""

    # Thresholds for disharmony classification
    THRESHOLDS = {
        DisharmonyLevel.HARMONIOUS: 0.3,
        DisharmonyLevel.MINOR_DRIFT: 0.5,
        DisharmonyLevel.CONCERNING: 0.8,
        DisharmonyLevel.SIGNIFICANT: 1.2,
    }

    def calculate(
        self,
        intent: LJPWCoords,
        execution: LJPWCoords
    ) -> float:
        """
        Calculate Euclidean distance between intent and execution in LJPW space.

        Formula: √[(Li-Le)² + (Ji-Je)² + (Pi-Pe)² + (Wi-We)²]

        Args:
            intent: Intent coordinates (L, J, P, W)
            execution: Execution coordinates (L, J, P, W)

        Returns:
            Disharmony score (distance)
        """
        l_i, j_i, p_i, w_i = intent
        l_e, j_e, p_e, w_e = execution

        distance = math.sqrt(
            (l_i - l_e) ** 2 +
            (j_i - j_e) ** 2 +
            (p_i - p_e) ** 2 +
            (w_i - w_e) ** 2
        )

        return distance

    def classify(self, disharmony_score: float) -> DisharmonyLevel:
        """
        Classify disharmony score into severity level.

        Args:
            disharmony_score: Calculated disharmony score

        Returns:
            DisharmonyLevel enum value
        """
        if disharmony_score < self.THRESHOLDS[DisharmonyLevel.HARMONIOUS]:
            return DisharmonyLevel.HARMONIOUS
        elif disharmony_score < self.THRESHOLDS[DisharmonyLevel.MINOR_DRIFT]:
            return DisharmonyLevel.MINOR_DRIFT
        elif disharmony_score < self.THRESHOLDS[DisharmonyLevel.CONCERNING]:
            return DisharmonyLevel.CONCERNING
        elif disharmony_score < self.THRESHOLDS[DisharmonyLevel.SIGNIFICANT]:
            return DisharmonyLevel.SIGNIFICANT
        else:
            return DisharmonyLevel.CRITICAL

    def get_trajectory(
        self,
        intent: LJPWCoords,
        execution: LJPWCoords
    ) -> Dict[str, float]:
        """
        Calculate dimensional trajectories (differences).

        Args:
            intent: Intent coordinates
            execution: Execution coordinates

        Returns:
            Dictionary of dimensional differences
        """
        l_i, j_i, p_i, w_i = intent
        l_e, j_e, p_e, w_e = execution

        return {
            'love_delta': l_e - l_i,
            'justice_delta': j_e - j_i,
            'power_delta': p_e - p_i,
            'wisdom_delta': w_e - w_i,
        }

    def get_dominant_shift(
        self,
        intent: LJPWCoords,
        execution: LJPWCoords
    ) -> Tuple[str, str]:
        """
        Identify the dominant semantic shift.

        Args:
            intent: Intent coordinates
            execution: Execution coordinates

        Returns:
            Tuple of (from_dimension, to_dimension)
        """
        dimensions = ['Love', 'Justice', 'Power', 'Wisdom']

        # Find dominant dimension in intent
        intent_max_idx = intent.index(max(intent))
        intent_dominant = dimensions[intent_max_idx]

        # Find dominant dimension in execution
        execution_max_idx = execution.index(max(execution))
        execution_dominant = dimensions[execution_max_idx]

        return (intent_dominant, execution_dominant)

    def explain_disharmony(
        self,
        intent: LJPWCoords,
        execution: LJPWCoords,
        disharmony_score: float
    ) -> str:
        """
        Generate human-readable explanation of disharmony.

        Args:
            intent: Intent coordinates
            execution: Execution coordinates
            disharmony_score: Calculated disharmony score

        Returns:
            Explanation string
        """
        level = self.classify(disharmony_score)
        from_dim, to_dim = self.get_dominant_shift(intent, execution)

        explanations = {
            DisharmonyLevel.HARMONIOUS:
                "Function name accurately reflects implementation. "
                "Intent and execution are well-aligned.",

            DisharmonyLevel.MINOR_DRIFT:
                f"Minor semantic drift detected. Function name suggests {from_dim} "
                f"operations, but implementation leans toward {to_dim}. "
                "Consider reviewing for clarity.",

            DisharmonyLevel.CONCERNING:
                f"Concerning contradiction detected. Function name strongly suggests "
                f"{from_dim} domain, but implementation operates in {to_dim} domain. "
                "This may confuse developers and cause bugs.",

            DisharmonyLevel.SIGNIFICANT:
                f"Significant semantic disharmony! Function name promises {from_dim} "
                f"behavior but delivers {to_dim} behavior. This is likely to cause "
                "production issues. Rename or refactor immediately.",

            DisharmonyLevel.CRITICAL:
                f"CRITICAL SEMANTIC BUG! Function name explicitly says it does "
                f"{from_dim} operations but actually performs {to_dim} operations. "
                f"This is a severe mismatch that will surprise developers and "
                "cause production bugs. Immediate action required!"
        }

        return explanations.get(
            level,
            "Unable to classify disharmony level."
        )

    def calculate_detailed_analysis(
        self,
        procedure_name: str,
        intent: LJPWCoords,
        execution: LJPWCoords
    ) -> Dict:
        """
        Perform complete disharmony analysis.

        Args:
            procedure_name: Name of the procedure
            intent: Intent coordinates
            execution: Execution coordinates

        Returns:
            Dictionary containing complete analysis
        """
        score = self.calculate(intent, execution)
        level = self.classify(score)
        trajectory = self.get_trajectory(intent, execution)
        from_dim, to_dim = self.get_dominant_shift(intent, execution)
        explanation = self.explain_disharmony(intent, execution, score)

        return {
            'procedure_name': procedure_name,
            'disharmony_score': round(score, 3),
            'severity_level': level.value,
            'intent_coords': {
                'love': round(intent[0], 3),
                'justice': round(intent[1], 3),
                'power': round(intent[2], 3),
                'wisdom': round(intent[3], 3),
            },
            'execution_coords': {
                'love': round(execution[0], 3),
                'justice': round(execution[1], 3),
                'power': round(execution[2], 3),
                'wisdom': round(execution[3], 3),
            },
            'trajectory': {
                'love_delta': round(trajectory['love_delta'], 3),
                'justice_delta': round(trajectory['justice_delta'], 3),
                'power_delta': round(trajectory['power_delta'], 3),
                'wisdom_delta': round(trajectory['wisdom_delta'], 3),
            },
            'dominant_shift': {
                'from': from_dim,
                'to': to_dim,
            },
            'explanation': explanation,
            'is_harmonious': level == DisharmonyLevel.HARMONIOUS,
            'requires_action': level in [
                DisharmonyLevel.SIGNIFICANT,
                DisharmonyLevel.CRITICAL
            ],
        }


# Singleton instance
_calculator_instance = None

def get_disharmony_calculator() -> DisharmonyCalculator:
    """Get singleton DisharmonyCalculator instance"""
    global _calculator_instance
    if _calculator_instance is None:
        _calculator_instance = DisharmonyCalculator()
    return _calculator_instance
