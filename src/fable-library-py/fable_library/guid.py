import uuid

def parse(string: str) -> uuid.UUID:
    return uuid.UUID(string)

def to_string(guid: uuid.UUID):
    return str(guid)