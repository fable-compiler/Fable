from datetime import timedelta

def total_seconds(ts):
    return ts / 1000

def from_milliseconds(msecs):
    return timedelta(milliseconds=msecs)

def to_milliseconds(td):
    return int(td.total_seconds() * 1000)