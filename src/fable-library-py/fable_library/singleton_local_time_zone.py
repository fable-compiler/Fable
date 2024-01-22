from __future__ import annotations

import time as _time
from datetime import datetime, timedelta, tzinfo


class LocalTimezone(tzinfo):
    def utcoffset(self, dt: datetime | None = None):
        # Return offset of local timezone from UTC
        if dt is not None and self._isdst(dt):
            return timedelta(seconds=-_time.altzone)
        else:
            return timedelta(seconds=-_time.timezone)

    def dst(self, dt: datetime | None = None):
        # Return the daylight saving time (DST) adjustment
        if dt is not None and self._isdst(dt):
            return timedelta(seconds=-_time.altzone)
        else:
            return timedelta(seconds=-_time.timezone)

    def tzname(self, dt: datetime | None = None) -> str:
        # Return the name of the time zone
        return "localtime"

    def _isdst(self, dt: datetime):
        # Determine whether or not DST is in effect
        tt = (dt.year, dt.month, dt.day, dt.hour, dt.minute, dt.second, dt.weekday(), 0, 0)
        stamp = _time.mktime(tt)
        tt = _time.localtime(stamp)
        return tt.tm_isdst > 0


local_time_zone = LocalTimezone()

__all__ = ["local_time_zone"]
