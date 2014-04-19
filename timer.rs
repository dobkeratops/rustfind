//#[crate_id = "timer"];
//#[crate_type = "lib"];

extern crate time;

static SEC_MULTIPLIER:u64 = 1000 * 1000 * 1000;
static MIN_MULTIPLIER:u64 = 60 * SEC_MULTIPLIER;
static HR_MULTIPLIER:u64 = 60 * MIN_MULTIPLIER;

pub struct Timer {
    start_time: u64,
    end_time: u64
}

impl Timer {
    pub fn new() -> Timer {
        Timer { start_time: 0, end_time: 0}
    }
    pub fn start(&mut self) -> () {
        self.start_time = time::precise_time_ns();
    }
    pub fn end(&mut self) -> () {
        self.end_time = time::precise_time_ns();
    }
    pub fn get_time_string(&self) -> ~str {
        return format_as_time(self.get_total_time());
    }
    pub fn get_total_time(&self) -> u64 {
        return self.end_time - self.start_time;
    }
    pub fn show_time(&self) -> () {
        println!("Total time: {:s}", self.get_time_string());
    }
    pub fn show_time_of(&self,txt:&str) -> () {
        println!("Total time:{:s}={:s}", txt,self.get_time_string());
    }
}

/// automatically profile a scopeblock, RAII based.
pub struct Profiler {
	time:Timer,
	name:~str,
}

static  mut g_depth:int=0;	// TODO - use an appropriate primitive, threadsafe, howto?

/// helper to automatically profile a scope block. place a "let p=Profiler::new("my_profiler")" at the begining of a scope, time is reported at the end using drop. Care would be needed if you were profiling other drops..
impl Profiler {
	// todo - cfg(profile)..
	pub fn new(n:&'static str)->Profiler{
		let mut p=Profiler { time: Timer::new(), name:n.to_owned()};
		p.time.start();
		unsafe {g_depth+=1;};
		p
	}
}
impl Drop for Profiler {
	fn drop(&mut self) {
		unsafe {g_depth-=1;};
		self.time.end();
		self.time.show_time_of(self.name);
	}
}

pub fn format_as_time(total_time: u64) -> ~str {
    let hours = total_time / HR_MULTIPLIER;
    let minutes = (total_time 
                   - hours * HR_MULTIPLIER) 
        / MIN_MULTIPLIER;
    let seconds = (total_time 
                   - hours * HR_MULTIPLIER 
                   - minutes * MIN_MULTIPLIER) 
        / SEC_MULTIPLIER;
    let nanoseconds = total_time 
                       - hours * HR_MULTIPLIER 
                       - minutes * MIN_MULTIPLIER 
                       - seconds * SEC_MULTIPLIER;

    let mut time_string = StrBuf::new();
    if hours > 0 {
        time_string.push_str(format!("{}:", hours as int));
    }
    if hours > 0 || minutes > 0 {
        if minutes < 10 && hours > 0 {
            time_string.push_str("0");
        }
        time_string.push_str(format!("{}:", minutes as int));
    }
    if hours > 0 || minutes > 0 || seconds > 0 {
        if seconds < 10 && (minutes > 0 || hours > 0) {
            // HACK: format!("%02?.", seconds) doesn't zero pad
            time_string.push_str("0");
        }
        time_string.push_str(format!("{}.", seconds as int));
        // nanoseconds don't need to be quite as accurate if we measure seconds
        let ns_as_string = format!("{:.5?}", (nanoseconds as f64) / (SEC_MULTIPLIER as f64));
        time_string.push_str(format!("{:s}", ns_as_string.slice(2, 5)));
    } else {
        time_string.push_str(format!("{:s}", format_number(nanoseconds)));
    }

    if hours > 0 {
        time_string.push_str(" hr");
    } else if minutes > 0 {
        time_string.push_str(" min");
    } else if seconds > 0 {
        time_string.push_str(" sec");
    } else {
        time_string.push_str(" ns");
    }

    //time_string += format!(" ({})", total_time);

    return time_string.as_slice().to_owned();
}

fn format_number(num: u64) -> ~str {
    let repr = num.to_str();
    let mut ret_val = StrBuf::new();
    let mut index = 0;
    let length = repr.len();

    while index < length {
        ret_val.push_str(repr.slice(index, index + 1));

        if (length - index - 1) % 3 == 0 && length > index + 1{
            ret_val.push_str(",");
        }
        index += 1;
    }

    return ret_val.as_slice().to_owned()
}

#[test]
fn format_number_test() {
    let num1 = 123456789;
    let num2 = 12345678;
    let num3 = 1234;

    assert!(format_number(num1) == ~"123,456,789");
    assert!(format_number(num2) == ~"12,345,678");
    assert!(format_number(num3) == ~"1,234");
}

#[test]
fn format_as_time_test() {
    let num1 = 2000;    // ns
    let num2 = 3 * SEC_MULTIPLIER + 141591234;
    let num3 = 1 * MIN_MULTIPLIER + 5 * SEC_MULTIPLIER + 98765432;
    let num4 = 3 * HR_MULTIPLIER + num3;

    assert!(format_as_time(num1) == ~"2,000 ns");
    assert!(format_as_time(num2) == ~"3.141 sec");
    assert!(format_as_time(num3) == ~"1:05.098 min");
    assert!(format_as_time(num4) == ~"3:01:05.098 hr");
}
