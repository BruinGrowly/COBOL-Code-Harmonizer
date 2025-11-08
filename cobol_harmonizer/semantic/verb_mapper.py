"""
COBOL Verb Mapper

Maps COBOL verbs to LJPW (Love, Justice, Power, Wisdom) semantic coordinates.
Based on comprehensive COBOL verb taxonomy.
"""

from typing import Tuple, Dict

# Type alias for LJPW coordinates
LJPWCoords = Tuple[float, float, float, float]


class VerbMapper:
    """Maps COBOL verbs to semantic coordinates in LJPW space"""

    # Complete COBOL verb to LJPW coordinate mapping
    VERB_COORDINATES: Dict[str, LJPWCoords] = {
        # WISDOM-Dominant Verbs (Information/Knowledge)
        # Format: (Love, Justice, Power, Wisdom)
        "READ": (0.1, 0.2, 0.0, 0.7),
        "ACCEPT": (0.1, 0.1, 0.0, 0.8),
        "COMPUTE": (0.0, 0.3, 0.1, 0.6),
        "ADD": (0.0, 0.3, 0.2, 0.5),
        "SUBTRACT": (0.0, 0.3, 0.2, 0.5),
        "MULTIPLY": (0.0, 0.3, 0.2, 0.5),
        "DIVIDE": (0.0, 0.3, 0.2, 0.5),
        "INSPECT": (0.0, 0.2, 0.0, 0.8),
        "STRING": (0.0, 0.2, 0.1, 0.7),
        "UNSTRING": (0.0, 0.2, 0.1, 0.7),
        "DISPLAY": (0.3, 0.1, 0.1, 0.5),
        # JUSTICE-Dominant Verbs (Validation/Verification)
        "IF": (0.0, 0.8, 0.1, 0.1),
        "EVALUATE": (0.0, 0.8, 0.1, 0.1),
        "SEARCH": (0.0, 0.7, 0.0, 0.3),
        "TEST": (0.0, 0.9, 0.0, 0.1),
        "VALIDATE": (0.0, 0.9, 0.0, 0.1),
        # POWER-Dominant Verbs (State Modification)
        "MOVE": (0.0, 0.1, 0.6, 0.3),
        "WRITE": (0.1, 0.1, 0.7, 0.1),
        "REWRITE": (0.0, 0.2, 0.7, 0.1),
        "DELETE": (0.0, 0.2, 0.8, 0.0),
        "OPEN": (0.1, 0.1, 0.7, 0.1),
        "CLOSE": (0.1, 0.1, 0.7, 0.1),
        "INITIALIZE": (0.0, 0.1, 0.7, 0.2),
        "SET": (0.0, 0.1, 0.7, 0.2),
        "RELEASE": (0.0, 0.1, 0.8, 0.1),
        "FREE": (0.0, 0.1, 0.8, 0.1),
        "CANCEL": (0.0, 0.1, 0.7, 0.2),
        "ALTER": (0.0, 0.2, 0.8, 0.0),
        # LOVE-Dominant Verbs (Connection/Communication)
        "CALL": (0.5, 0.1, 0.3, 0.1),
        "INVOKE": (0.6, 0.1, 0.2, 0.1),
        "MERGE": (0.5, 0.2, 0.2, 0.1),
        "SORT": (0.4, 0.2, 0.2, 0.2),
        # BALANCED/CONTROL-FLOW Verbs
        "PERFORM": (0.2, 0.2, 0.4, 0.2),
        "CONTINUE": (0.0, 0.5, 0.5, 0.0),
        "EXIT": (0.0, 0.2, 0.6, 0.2),
        "GO": (0.0, 0.3, 0.5, 0.2),  # GO TO
        "GOBACK": (0.0, 0.2, 0.6, 0.2),
        "STOP": (0.0, 0.2, 0.6, 0.2),
        # SQL Embedded Verbs (for COBOL with embedded SQL)
        "SELECT": (0.1, 0.2, 0.0, 0.7),  # SQL SELECT
        "INSERT": (0.1, 0.2, 0.7, 0.0),  # SQL INSERT
        "UPDATE": (0.0, 0.2, 0.7, 0.1),  # SQL UPDATE
        # Additional I/O verbs
        "START": (0.1, 0.2, 0.5, 0.2),
        "RETURN": (0.1, 0.2, 0.1, 0.6),
        "ENABLE": (0.0, 0.1, 0.7, 0.2),
        "DISABLE": (0.0, 0.1, 0.7, 0.2),
        "SEND": (0.4, 0.1, 0.4, 0.1),
        "RECEIVE": (0.3, 0.1, 0.1, 0.5),
        # Report Writer verbs
        "GENERATE": (0.2, 0.1, 0.4, 0.3),
        "INITIATE": (0.1, 0.1, 0.6, 0.2),
        "TERMINATE": (0.0, 0.1, 0.7, 0.2),
        "SUPPRESS": (0.0, 0.2, 0.6, 0.2),
        # Debugging verbs (Enterprise COBOL)
        "EXHIBIT": (0.2, 0.1, 0.0, 0.7),
        "READY": (0.0, 0.2, 0.5, 0.3),
        "RESET": (0.0, 0.1, 0.7, 0.2),
        # Object-Oriented COBOL (COBOL 2002+)
        "CREATE": (0.1, 0.2, 0.6, 0.1),
        "DESTROY": (0.0, 0.1, 0.8, 0.1),
        "RAISE": (0.2, 0.3, 0.4, 0.1),  # Exception handling
        # Communication verbs
        "ACCEPT": (0.1, 0.1, 0.0, 0.8),
        "COMMIT": (0.1, 0.3, 0.5, 0.1),
        "ROLLBACK": (0.0, 0.3, 0.6, 0.1),
        # XML/JSON verbs (Enterprise COBOL)
        "XML": (0.2, 0.3, 0.2, 0.3),
        "JSON": (0.2, 0.3, 0.2, 0.3),
        "PARSE": (0.0, 0.3, 0.1, 0.6),
    }

    # Intent keywords for procedure names
    INTENT_KEYWORDS: Dict[str, LJPWCoords] = {
        # Wisdom-indicating keywords
        "GET": (0.1, 0.1, 0.0, 0.8),
        "FETCH": (0.1, 0.1, 0.0, 0.8),
        "READ": (0.1, 0.2, 0.0, 0.7),
        "RETRIEVE": (0.1, 0.1, 0.0, 0.8),
        "CALCULATE": (0.0, 0.3, 0.1, 0.6),
        "COMPUTE": (0.0, 0.3, 0.1, 0.6),
        "DISPLAY": (0.3, 0.1, 0.0, 0.6),
        "SHOW": (0.2, 0.1, 0.0, 0.7),
        "PRINT": (0.2, 0.1, 0.2, 0.5),
        "FORMAT": (0.1, 0.2, 0.1, 0.6),
        "FIND": (0.0, 0.5, 0.0, 0.5),
        # Justice-indicating keywords
        "VALIDATE": (0.0, 0.8, 0.1, 0.1),
        "VERIFY": (0.0, 0.8, 0.1, 0.1),
        "CHECK": (0.0, 0.7, 0.1, 0.2),
        "TEST": (0.0, 0.8, 0.1, 0.1),
        "AUDIT": (0.0, 0.7, 0.1, 0.2),
        "CONFIRM": (0.0, 0.7, 0.2, 0.1),
        "INSPECT": (0.0, 0.6, 0.0, 0.4),
        "SEARCH": (0.0, 0.7, 0.0, 0.3),
        # Power-indicating keywords
        "UPDATE": (0.0, 0.2, 0.7, 0.1),
        "DELETE": (0.0, 0.2, 0.8, 0.0),
        "REMOVE": (0.0, 0.2, 0.8, 0.0),
        "CREATE": (0.1, 0.2, 0.6, 0.1),
        "INSERT": (0.0, 0.2, 0.7, 0.1),
        "WRITE": (0.1, 0.1, 0.7, 0.1),
        "SET": (0.0, 0.1, 0.8, 0.1),
        "INITIALIZE": (0.0, 0.1, 0.7, 0.2),
        "RESET": (0.0, 0.1, 0.7, 0.2),
        "CLEAR": (0.0, 0.1, 0.8, 0.1),
        "OPEN": (0.1, 0.1, 0.7, 0.1),
        "CLOSE": (0.1, 0.1, 0.7, 0.1),
        "PURGE": (0.0, 0.1, 0.9, 0.0),
        "MODIFY": (0.0, 0.2, 0.7, 0.1),
        "CHANGE": (0.0, 0.2, 0.7, 0.1),
        # Love-indicating keywords
        "LINK": (0.6, 0.1, 0.2, 0.1),
        "CONNECT": (0.7, 0.1, 0.1, 0.1),
        "MERGE": (0.5, 0.2, 0.2, 0.1),
        "COMBINE": (0.5, 0.1, 0.3, 0.1),
        "JOIN": (0.6, 0.1, 0.2, 0.1),
        "ASSOCIATE": (0.6, 0.1, 0.2, 0.1),
        "CALL": (0.5, 0.1, 0.3, 0.1),
        # Ambiguous/Vague keywords (code smells)
        "PROCESS": (0.1, 0.2, 0.4, 0.3),
        "HANDLE": (0.1, 0.2, 0.4, 0.3),
        "MANAGE": (0.2, 0.2, 0.3, 0.3),
        "EXECUTE": (0.1, 0.2, 0.5, 0.2),
        "DO": (0.1, 0.1, 0.6, 0.2),
        "RUN": (0.1, 0.2, 0.5, 0.2),
        "PERFORM": (0.2, 0.2, 0.4, 0.2),
    }

    # Default coordinate for unknown verbs
    DEFAULT_COORDS: LJPWCoords = (0.25, 0.25, 0.25, 0.25)

    def map_verb(self, verb: str) -> LJPWCoords:
        """
        Map a COBOL verb to its LJPW semantic coordinates.

        Args:
            verb: COBOL verb (e.g., 'READ', 'WRITE', 'COMPUTE')

        Returns:
            Tuple of (Love, Justice, Power, Wisdom) coordinates
        """
        verb_upper = verb.upper().strip()
        return self.VERB_COORDINATES.get(verb_upper, self.DEFAULT_COORDS)

    def map_intent_keyword(self, keyword: str) -> LJPWCoords:
        """
        Map a procedure name keyword to its intended LJPW semantics.

        Args:
            keyword: Keyword from procedure name (e.g., 'GET', 'VALIDATE')

        Returns:
            Tuple of (Love, Justice, Power, Wisdom) coordinates
        """
        keyword_upper = keyword.upper().strip()
        return self.INTENT_KEYWORDS.get(keyword_upper, self.DEFAULT_COORDS)

    def is_vague_keyword(self, keyword: str) -> bool:
        """
        Check if a keyword is vague/ambiguous (code smell).

        Args:
            keyword: Keyword to check

        Returns:
            True if keyword is vague (PROCESS, HANDLE, etc.)
        """
        vague_keywords = {"PROCESS", "HANDLE", "MANAGE", "EXECUTE", "DO", "RUN"}
        return keyword.upper().strip() in vague_keywords

    def get_dominant_dimension(self, coords: LJPWCoords) -> str:
        """
        Get the dominant semantic dimension from coordinates.

        Args:
            coords: LJPW coordinates

        Returns:
            Name of dominant dimension ('Love', 'Justice', 'Power', or 'Wisdom')
        """
        dimensions = ["Love", "Justice", "Power", "Wisdom"]
        max_index = coords.index(max(coords))
        return dimensions[max_index]

    def describe_semantics(self, coords: LJPWCoords) -> str:
        """
        Generate human-readable description of semantic profile.

        Args:
            coords: LJPW coordinates

        Returns:
            Description string
        """
        l, j, p, w = coords
        dominant = self.get_dominant_dimension(coords)

        descriptions = {
            "Love": "connection/communication-focused",
            "Justice": "validation/verification-focused",
            "Power": "state modification/action-focused",
            "Wisdom": "information/knowledge-focused",
        }

        return f"{dominant}-dominant ({descriptions[dominant]})"


# Singleton instance
_mapper_instance = None


def get_verb_mapper() -> VerbMapper:
    """Get singleton VerbMapper instance"""
    global _mapper_instance
    if _mapper_instance is None:
        _mapper_instance = VerbMapper()
    return _mapper_instance
