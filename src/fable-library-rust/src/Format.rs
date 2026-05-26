pub mod Format {
    fn invalid_format() -> ! {
        panic!("Input string was not in a correct format.")
    }

    #[cfg(feature = "datetime")]
    pub mod DateTime {
        use super::invalid_format;
        use crate::{
            DateOnly_::DateOnly,
            DateTime_::{DateTime, DateTimeKind},
            Native_::{String, ToString, Vec, format},
            TimeSpan_::nanoseconds_per_tick,
        };
        use chrono::{
            DateTime as CDateTime, Datelike, FixedOffset, Local, NaiveDate, NaiveDateTime,
            NaiveTime, ParseResult, TimeZone,
        };

        fn skip_ascii_whitespace(bytes: &[u8], cursor: &mut usize) {
            while *cursor < bytes.len() && bytes[*cursor].is_ascii_whitespace() {
                *cursor += 1;
            }
        }

        fn consume_byte(bytes: &[u8], cursor: &mut usize, expected: u8) -> bool {
            if *cursor < bytes.len() && bytes[*cursor] == expected {
                *cursor += 1;
                true
            } else {
                false
            }
        }

        fn parse_ascii_i32(
            s: &str,
            bytes: &[u8],
            cursor: &mut usize,
            max_digits: usize,
        ) -> Option<i32> {
            let start = *cursor;
            while *cursor < bytes.len() && bytes[*cursor].is_ascii_digit() {
                *cursor += 1;
            }

            if start == *cursor || *cursor - start > max_digits {
                None
            } else {
                s[start..*cursor].parse().ok()
            }
        }

        fn split_numeric_fields(input: &str, separators: &[u8]) -> Option<Vec<String>> {
            let bytes = input.as_bytes();
            let mut cursor = 0;
            let mut fields = Vec::new();

            skip_ascii_whitespace(bytes, &mut cursor);
            if cursor == bytes.len() {
                return None;
            }

            loop {
                let start = cursor;
                while cursor < bytes.len() && bytes[cursor].is_ascii_digit() {
                    cursor += 1;
                }
                if start == cursor {
                    return None;
                }

                fields.push(input[start..cursor].to_string());

                skip_ascii_whitespace(bytes, &mut cursor);
                if cursor == bytes.len() {
                    break;
                }

                if !separators.contains(&bytes[cursor]) {
                    return None;
                }

                cursor += 1;
                skip_ascii_whitespace(bytes, &mut cursor);

                if cursor == bytes.len() {
                    return None;
                }
            }

            Some(fields)
        }

        pub(crate) fn split_meridiem_suffix(s: &str) -> (&str, Option<&str>) {
            let trimmed = s.trim_end();

            if trimmed.len() >= 2 {
                let suffix = &trimmed[trimmed.len() - 2..];
                if suffix.eq_ignore_ascii_case("AM") || suffix.eq_ignore_ascii_case("PM") {
                    return (trimmed[..trimmed.len() - 2].trim_end(), Some(suffix));
                }
            }

            (trimmed, None)
        }

        pub(crate) fn split_date_fields(s: &str) -> Option<Vec<String>> {
            split_numeric_fields(s, &[b'.', b',', b'/', b'-'])
        }

        pub(crate) fn try_parse_date_str(s: &str) -> Option<NaiveDate> {
            let parts = split_date_fields(s)?;

            let (year, month, day) = match parts.as_slice() {
                [first, second] => {
                    let first = first.as_str();
                    let second = second.as_str();

                    if first.len() < 3 {
                        let month: i32 = first.parse().ok()?;

                        if second.len() < 3 {
                            (Local::now().year(), month, second.parse().ok()?)
                        } else {
                            (second.parse().ok()?, month, 1)
                        }
                    } else {
                        if second.len() > 2 {
                            return None;
                        }

                        (first.parse().ok()?, second.parse().ok()?, 1)
                    }
                }
                [first, second, third] => {
                    let year_first = first.len() > 2;
                    let year_text = if year_first {
                        first.as_str()
                    } else {
                        third.as_str()
                    };
                    let mut year: i32 = year_text.parse().ok()?;

                    if year_text.len() < 3 {
                        year += if year >= 30 { 1900 } else { 2000 };
                    }

                    let month: i32 = if year_first {
                        second.parse().ok()?
                    } else {
                        first.parse().ok()?
                    };
                    let day: i32 = if year_first {
                        third.parse().ok()?
                    } else {
                        second.parse().ok()?
                    };

                    (year, month, day)
                }
                _ => return None,
            };

            if year <= 0 || month <= 0 || day <= 0 {
                return None;
            }

            NaiveDate::from_ymd_opt(year, month as u32, day as u32)
        }

        pub(crate) fn try_parse_time_str(s: &str, meridiem: Option<&str>) -> Option<NaiveTime> {
            let bytes = s.as_bytes();
            let mut cursor = 0;

            skip_ascii_whitespace(bytes, &mut cursor);

            let mut hour = parse_ascii_i32(s, bytes, &mut cursor, 2)?;

            skip_ascii_whitespace(bytes, &mut cursor);
            if !consume_byte(bytes, &mut cursor, b':') {
                return None;
            }

            skip_ascii_whitespace(bytes, &mut cursor);

            let minute = parse_ascii_i32(s, bytes, &mut cursor, 2)?;

            let mut second = 0;
            let mut nanosecond = 0;

            skip_ascii_whitespace(bytes, &mut cursor);

            if cursor < bytes.len() {
                if !consume_byte(bytes, &mut cursor, b':') {
                    return None;
                }
                skip_ascii_whitespace(bytes, &mut cursor);

                second = parse_ascii_i32(s, bytes, &mut cursor, 2)?;

                if consume_byte(bytes, &mut cursor, b'.') {
                    let fraction_start = cursor;
                    while cursor < bytes.len() && bytes[cursor].is_ascii_digit() {
                        cursor += 1;
                    }
                    if fraction_start == cursor {
                        return None;
                    }

                    let mut ticks = s[fraction_start..cursor]
                        .chars()
                        .take(7)
                        .collect::<String>();
                    while ticks.len() < 7 {
                        ticks.push('0');
                    }
                    nanosecond = ticks.parse::<u32>().ok()? * nanoseconds_per_tick as u32;
                }

                skip_ascii_whitespace(bytes, &mut cursor);
            }

            if cursor != bytes.len() || minute > 59 || second > 59 {
                return None;
            }

            match meridiem {
                Some(value) if value.eq_ignore_ascii_case("AM") => {
                    if hour == 12 {
                        hour = 0;
                    } else if hour == 0 || hour > 11 {
                        return None;
                    }
                }
                Some(value) if value.eq_ignore_ascii_case("PM") => {
                    if hour == 0 || hour > 12 {
                        return None;
                    }
                    if hour < 12 {
                        hour += 12;
                    }
                }
                Some(_) => return None,
                None => {
                    if hour > 23 {
                        return None;
                    }
                }
            }

            NaiveTime::from_hms_nano_opt(hour as u32, minute as u32, second as u32, nanosecond)
        }

        pub(crate) fn try_parse_offset_suffix_minutes(suffix: &str) -> Option<i32> {
            match suffix.as_bytes() {
                [b'Z'] | [b'z'] => Some(0),
                [sign @ (b'+' | b'-'), rest @ ..] => {
                    let rest = core::str::from_utf8(rest).ok()?;
                    let (hours_text, minutes_text) = match rest.split_once(':') {
                        Some((hours, minutes)) => (hours, Some(minutes)),
                        None => (rest, None),
                    };

                    if hours_text.is_empty()
                        || hours_text.len() > 2
                        || !hours_text.bytes().all(|byte| byte.is_ascii_digit())
                    {
                        return None;
                    }

                    let hours: i32 = hours_text.parse().ok()?;
                    let minutes: i32 = match minutes_text {
                        Some(value)
                            if !value.is_empty()
                                && value.len() <= 2
                                && value.bytes().all(|byte| byte.is_ascii_digit()) =>
                        {
                            value.parse().ok()?
                        }
                        Some(_) => return None,
                        None => 0,
                    };

                    let total_minutes = hours * 60 + minutes;
                    if minutes > 59 || total_minutes > 14 * 60 {
                        return None;
                    }

                    Some(if *sign == b'-' {
                        -total_minutes
                    } else {
                        total_minutes
                    })
                }
                _ => None,
            }
        }

        pub(crate) fn format_offset_token(offset_minutes: i32, token_length: usize) -> String {
            let sign = if offset_minutes < 0 { '-' } else { '+' };
            let total_minutes = offset_minutes.abs();
            let hours = total_minutes / 60;
            let minutes = total_minutes % 60;

            match token_length {
                1 => format!("{}{hours}", sign),
                2 => format!("{}{:02}", sign, hours),
                _ => format!("{}{:02}:{:02}", sign, hours, minutes),
            }
        }

        fn fraction_to_string(fraction_ticks: i64) -> String {
            format!("{:07}", fraction_ticks.abs())
        }

        fn format_year_token(year: i32, token_length: usize) -> String {
            match token_length {
                1 => (year.rem_euclid(100)).to_string(),
                2 => format!("{:02}", year.rem_euclid(100)),
                _ => format!("{:0width$}", year, width = token_length),
            }
        }

        fn format_z_token(suffix: &str, token_length: usize) -> String {
            if suffix.is_empty() {
                String::new()
            } else {
                try_parse_offset_suffix_minutes(suffix)
                    .map(|offset_minutes| format_offset_token(offset_minutes, token_length.min(3)))
                    .unwrap_or_else(|| invalid_format())
            }
        }

        fn format_run(
            ch: u8,
            run_len: usize,
            ndt: &NaiveDateTime,
            frac: &str,
            meridiem: &str,
            suffix: &str,
        ) -> String {
            match ch {
                b'd' => match run_len.min(4) {
                    1 => ndt.format("%-d").to_string(),
                    2 => ndt.format("%d").to_string(),
                    3 => ndt.format("%a").to_string(),
                    _ => ndt.format("%A").to_string(),
                },
                b'f' => frac[..run_len.min(7)].to_string(),
                b'F' => frac[..run_len.min(7)].trim_end_matches('0').to_string(),
                b'g' => "A.D.".to_string(),
                b'h' => {
                    if run_len >= 2 {
                        ndt.format("%I").to_string()
                    } else {
                        ndt.format("%-I").to_string()
                    }
                }
                b'H' => {
                    if run_len >= 2 {
                        ndt.format("%H").to_string()
                    } else {
                        ndt.format("%-H").to_string()
                    }
                }
                b'K' => suffix.to_string(),
                b'm' => {
                    if run_len >= 2 {
                        ndt.format("%M").to_string()
                    } else {
                        ndt.format("%-M").to_string()
                    }
                }
                b'M' => match run_len.min(4) {
                    1 => ndt.format("%-m").to_string(),
                    2 => ndt.format("%m").to_string(),
                    3 => ndt.format("%b").to_string(),
                    _ => ndt.format("%B").to_string(),
                },
                b's' => {
                    if run_len >= 2 {
                        ndt.format("%S").to_string()
                    } else {
                        ndt.format("%-S").to_string()
                    }
                }
                b't' => {
                    if run_len >= 2 {
                        meridiem.to_string()
                    } else {
                        meridiem.chars().next().unwrap_or_default().to_string()
                    }
                }
                b'y' => format_year_token(ndt.year(), run_len),
                b'z' => format_z_token(suffix, run_len.min(3)),
                _ => (0..run_len).map(|_| ch as char).collect(),
            }
        }

        pub(crate) fn format_dotnet_custom(
            ndt: NaiveDateTime,
            fraction_ticks: i64,
            suffix: &str,
            format: &str,
        ) -> String {
            let frac = fraction_to_string(fraction_ticks);
            let meridiem = ndt.format("%p").to_string();

            let mut result = String::with_capacity(format.len() * 2);
            let bytes = format.as_bytes();
            let mut i = 0;

            while i < bytes.len() {
                match bytes[i] {
                    b'\'' | b'"' => {
                        let quote = bytes[i];
                        i += 1;
                        while i < bytes.len() && bytes[i] != quote {
                            if bytes[i] == b'\\' {
                                i += 1;
                                if i >= bytes.len() {
                                    invalid_format();
                                }
                            }
                            result.push(bytes[i] as char);
                            i += 1;
                        }
                        if i >= bytes.len() {
                            invalid_format();
                        }
                        i += 1;
                    }
                    b'\\' => {
                        i += 1;
                        if i >= bytes.len() {
                            invalid_format();
                        }
                        result.push(bytes[i] as char);
                        i += 1;
                    }
                    b'%' => {
                        i += 1;
                        if i >= bytes.len() {
                            invalid_format();
                        }
                        let ch = bytes[i];
                        i += 1;
                        if !ch.is_ascii_alphabetic() {
                            invalid_format();
                        }
                        let value = format_run(ch, 1, &ndt, &frac, &meridiem, suffix);
                        result.push_str(&value);
                    }
                    ch if ch.is_ascii_alphabetic() => {
                        let run_start = i;
                        while i < bytes.len() && bytes[i] == ch {
                            i += 1;
                        }
                        let run_len = i - run_start;
                        let value = format_run(ch, run_len, &ndt, &frac, &meridiem, suffix);
                        result.push_str(&value);
                    }
                    b'.' => {
                        let j = i + 1;
                        if j < bytes.len() && bytes[j] == b'F' {
                            let mut end = j;
                            while end < bytes.len() && bytes[end] == b'F' {
                                end += 1;
                            }
                            let f_value = format_run(b'F', end - j, &ndt, &frac, &meridiem, suffix);
                            if !f_value.is_empty() {
                                result.push('.');
                                result.push_str(&f_value);
                            }
                            i = end;
                        } else {
                            result.push('.');
                            i += 1;
                        }
                    }
                    _ => {
                        result.push(bytes[i] as char);
                        i += 1;
                    }
                }
            }

            result
        }

        pub(crate) fn format_roundtrip_datetime(
            ndt: NaiveDateTime,
            fraction_ticks: i64,
            suffix: &str,
        ) -> String {
            format!(
                "{}.{:07}{}",
                ndt.format("%Y-%m-%dT%H:%M:%S"),
                fraction_ticks.abs(),
                suffix
            )
        }

        pub(crate) fn try_split_date_time(s: &str) -> Option<(&str, &str)> {
            if let Some(index) = s.find('T') {
                let date = s[..index].trim_end();
                let time = s[index + 1..].trim_start();
                return (!date.is_empty() && !time.is_empty()).then_some((date, time));
            }

            let bytes = s.as_bytes();
            for index in (0..bytes.len()).rev() {
                if bytes[index].is_ascii_whitespace() {
                    let date = s[..index].trim_end();
                    let time = s[index + 1..].trim_start();
                    if !date.is_empty() && !time.is_empty() && DateOnly::try_parse_str(date).is_ok()
                    {
                        return Some((date, time));
                    }
                }
            }

            None
        }

        fn parse_flexible_naive_datetime(
            body: &str,
            meridiem: Option<&str>,
        ) -> Option<NaiveDateTime> {
            if body.is_empty() {
                return None;
            }

            let (date, time) = if let Some((date_text, time_text)) = try_split_date_time(body) {
                let date = DateOnly::try_parse_str(date_text).ok()?;
                let time = try_parse_time_str(time_text, meridiem)?;
                (date, time)
            } else if body.contains(':') {
                (
                    Local::now().naive_local().date(),
                    try_parse_time_str(body, meridiem)?,
                )
            } else {
                (DateOnly::try_parse_str(body).ok()?, NaiveTime::MIN)
            };

            Some(date.and_time(time))
        }

        fn try_parse_datetime_flexible_str(s: &str) -> Option<DateTime> {
            let (body, meridiem) = split_meridiem_suffix(s);
            parse_flexible_naive_datetime(body.trim(), meridiem)
                .map(|ndt| DateTime::new(ndt, DateTimeKind::Unspecified))
        }

        pub(crate) fn try_parse_datetime_str(s: &str) -> ParseResult<DateTime> {
            s.parse::<NaiveDateTime>()
                .or(NaiveDateTime::parse_from_str(s, "%m/%d/%Y %H:%M:%S%.f"))
                .or(NaiveDateTime::parse_from_str(s, "%m/%d/%Y %I:%M:%S %P"))
                .map(|ndt| DateTime::new(ndt, DateTimeKind::Unspecified))
                .or_else(|_| {
                    try_parse_datetime_flexible_str(s).ok_or_else(|| {
                        NaiveDateTime::parse_from_str("", "%Y-%m-%dT%H:%M:%S").unwrap_err()
                    })
                })
                .or_else(|_| {
                    try_parse_datetime_offset_str(s).map(|cdt| {
                        let ndt = Local.from_utc_datetime(&cdt.naive_utc()).naive_local();
                        DateTime::new(ndt, DateTimeKind::Local)
                    })
                })
        }

        fn local_fixed_from_naive(ndt: NaiveDateTime) -> CDateTime<FixedOffset> {
            match Local.from_local_datetime(&ndt) {
                chrono::LocalResult::Single(dt) => dt.into(),
                chrono::LocalResult::Ambiguous(earliest, _) => earliest.into(),
                chrono::LocalResult::None => Local.from_utc_datetime(&ndt).into(),
            }
        }

        fn local_time_from_str(s: &str, fmt: &str) -> ParseResult<CDateTime<FixedOffset>> {
            let ndt = NaiveDateTime::parse_from_str(s, fmt)?;
            Ok(local_fixed_from_naive(ndt))
        }

        fn split_offset_suffix(s: &str) -> Option<(&str, Option<i32>)> {
            let trimmed = s.trim();
            if trimmed.is_empty() {
                return None;
            }

            if trimmed.ends_with('Z') || trimmed.ends_with('z') {
                let body = trimmed[..trimmed.len() - 1].trim_end();
                return (!body.is_empty()).then_some((body, Some(0)));
            }

            let bytes = trimmed.as_bytes();
            for index in (0..bytes.len()).rev() {
                if bytes[index] == b'+' || bytes[index] == b'-' {
                    if let Some(offset_minutes) = try_parse_offset_suffix_minutes(&trimmed[index..])
                    {
                        let body = trimmed[..index].trim_end();
                        if body.is_empty() {
                            return None;
                        }

                        let separated = index > 0 && bytes[index - 1].is_ascii_whitespace();
                        let attached_to_time = body.contains('T') || body.contains(':');
                        let date_components = split_date_fields(body)
                            .map(|parts| parts.len())
                            .unwrap_or_default();

                        if separated || attached_to_time || date_components == 3 {
                            return Some((body, Some(offset_minutes)));
                        }
                    }
                }
            }

            Some((trimmed, None))
        }

        fn try_parse_datetime_offset_manual_str(s: &str) -> Option<CDateTime<FixedOffset>> {
            let (body, offset_minutes) = split_offset_suffix(s)?;
            let (body, meridiem) = split_meridiem_suffix(body);
            let ndt = parse_flexible_naive_datetime(body.trim(), meridiem)?;

            match offset_minutes {
                Some(offset_minutes) => FixedOffset::east_opt(offset_minutes * 60)?
                    .from_local_datetime(&ndt)
                    .single(),
                None => Some(local_fixed_from_naive(ndt)),
            }
        }

        pub(crate) fn try_parse_datetime_offset_str(
            s: &str,
        ) -> ParseResult<CDateTime<FixedOffset>> {
            s.parse::<CDateTime<FixedOffset>>()
                .or(CDateTime::parse_from_str(s, "%m/%d/%Y %H:%M:%S%.f %#z"))
                .or_else(|_| local_time_from_str(s, "%m/%d/%Y %H:%M:%S%.f"))
                .or_else(|_| local_time_from_str(s, "%m/%d/%Y %I:%M:%S %P"))
                .or_else(|_| {
                    try_parse_datetime_offset_manual_str(s).ok_or_else(|| {
                        CDateTime::parse_from_str("", "%Y-%m-%dT%H:%M:%S%:z").unwrap_err()
                    })
                })
        }
    }

    pub mod TimeSpan {
        use super::invalid_format;
        use crate::{
            Native_::{String, ToString, Vec, format},
            TimeSpan_::{
                TimeSpan, ticks_per_day, ticks_per_hour, ticks_per_minute, ticks_per_second,
            },
        };

        fn format_component(value: i32, width: usize) -> String {
            if width <= 1 {
                value.to_string()
            } else {
                format!("{:0width$}", value, width = width)
            }
        }

        fn format_fraction(fraction: i64) -> String {
            format!("{:07}", fraction.abs())
        }

        fn format_custom_run(
            ch: u8,
            run_len: usize,
            days: i32,
            hours: i32,
            minutes: i32,
            seconds: i32,
            fraction: &str,
        ) -> String {
            match ch {
                b'd' => format_component(days, run_len),
                b'h' => format_component(hours, run_len),
                b'm' => format_component(minutes, run_len),
                b's' => format_component(seconds, run_len),
                b'f' => {
                    if run_len > 7 {
                        invalid_format();
                    }
                    fraction[..run_len].to_string()
                }
                b'F' => {
                    if run_len > 7 {
                        invalid_format();
                    }
                    fraction[..run_len].trim_end_matches('0').to_string()
                }
                _ => invalid_format(),
            }
        }

        fn format_custom_time_span(ts: &TimeSpan, format: &str) -> String {
            let sign = if ts.ticks() < 0 { "-" } else { "" };
            let duration = ts.duration();
            let days = duration.days();
            let hours = duration.hours();
            let minutes = duration.minutes();
            let seconds = duration.seconds();
            let fraction = format_fraction(duration.ticks() % ticks_per_second);

            let mut result = String::with_capacity(format.len() * 2 + sign.len());
            result.push_str(sign);

            let bytes = format.as_bytes();
            let mut i = 0;

            while i < bytes.len() {
                match bytes[i] {
                    b'\'' | b'"' => {
                        let quote = bytes[i];
                        i += 1;

                        while i < bytes.len() && bytes[i] != quote {
                            if bytes[i] == b'\\' {
                                i += 1;
                                if i >= bytes.len() {
                                    invalid_format();
                                }
                            }

                            result.push(bytes[i] as char);
                            i += 1;
                        }

                        if i >= bytes.len() {
                            invalid_format();
                        }

                        i += 1;
                    }
                    b'\\' => {
                        i += 1;
                        if i >= bytes.len() {
                            invalid_format();
                        }

                        result.push(bytes[i] as char);
                        i += 1;
                    }
                    b'%' => {
                        i += 1;
                        if i >= bytes.len() {
                            invalid_format();
                        }

                        let ch = bytes[i];
                        i += 1;
                        if !ch.is_ascii_alphabetic() {
                            invalid_format();
                        }

                        let value =
                            format_custom_run(ch, 1, days, hours, minutes, seconds, &fraction);
                        result.push_str(&value);
                    }
                    ch if ch.is_ascii_alphabetic() => {
                        let run_start = i;
                        while i < bytes.len() && bytes[i] == ch {
                            i += 1;
                        }

                        let run_len = i - run_start;
                        let value = format_custom_run(
                            ch, run_len, days, hours, minutes, seconds, &fraction,
                        );
                        result.push_str(&value);
                    }
                    _ => {
                        result.push(bytes[i] as char);
                        i += 1;
                    }
                }
            }

            result
        }

        pub(crate) fn format_time_span(ts: &TimeSpan, format: &str) -> String {
            let sign = if ts.ticks() < 0 { "-" } else { "" };
            let duration = ts.duration();
            let days = duration.days();
            let hours = duration.hours();
            let mins = duration.minutes();
            let secs = duration.seconds();
            let frac = duration.ticks() % ticks_per_second;
            let days_for_c = if days == 0 {
                "".to_string()
            } else {
                format_args!("{}.", days).to_string()
            };
            let frac_for_c = if frac == 0 {
                "".to_string()
            } else {
                format_args!(".{:07}", frac).to_string()
            };
            let frac_for_g = if frac == 0 {
                "".to_string()
            } else {
                let mut fraction = format_args!("{:07}", frac).to_string();

                while fraction.ends_with('0') {
                    fraction.pop();
                }

                format_args!(".{}", fraction).to_string()
            };

            match format {
                "" | "c" => format_args!(
                    "{}{}{:02}:{:02}:{:02}{}",
                    sign, days_for_c, hours, mins, secs, frac_for_c
                )
                .to_string(),
                "g" => {
                    let days = if days == 0 {
                        "".to_string()
                    } else {
                        format_args!("{}:", days).to_string()
                    };

                    format_args!(
                        "{}{}{}:{:02}:{:02}{}",
                        sign, days, hours, mins, secs, frac_for_g
                    )
                    .to_string()
                }
                "G" => format_args!(
                    "{}{}:{:02}:{:02}:{:02}.{:07}",
                    sign, days, hours, mins, secs, frac
                )
                .to_string(),
                _ => format_custom_time_span(ts, format),
            }
        }

        pub(crate) fn try_parse_time_span_str(s: &str) -> Result<TimeSpan, ()> {
            let error = Err(());
            let s = s.trim();
            let is_neg = s.starts_with('-');
            let s = if is_neg { &s[1..] } else { s };
            let hms = s.split(':').collect::<Vec<&str>>();
            if s.contains('-') || hms.len() > 3 {
                error
            } else {
                let (d, h, m, s) = if hms.len() == 1 {
                    (hms[0], "0", "0", "0")
                } else {
                    let (d, h) = if let Some(dh) = hms[0].split_once('.') {
                        dh
                    } else {
                        ("0", hms[0])
                    };
                    let m = hms[1];
                    let s = if hms.len() > 2 { hms[2] } else { "0" };
                    (d, h, m, s)
                };
                let d = d.parse::<u32>();
                let h = h.parse::<u32>();
                let m = m.parse::<u32>();
                let s = s.parse::<f64>();
                match (d, h, m, s) {
                    (Ok(d), Ok(h), Ok(m), Ok(s))
                        if d < 10675200 && h < 24 && m < 60 && s < 60.0 =>
                    {
                        let ticks = d as i64 * ticks_per_day
                            + h as i64 * ticks_per_hour
                            + m as i64 * ticks_per_minute
                            + (s * ticks_per_second as f64) as i64;
                        let ticks = if is_neg { -ticks } else { ticks };
                        Ok(TimeSpan::fromTicks(ticks))
                    }
                    _ => error,
                }
            }
        }
    }
}
