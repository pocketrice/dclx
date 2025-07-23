import sys
import pandas as pd
import matplotlib.pyplot as plt
import datetime as dt
from enum import Enum
from datetime import datetime
from matplotlib.dates import DateFormatter

PT_GAP_MOD_OFFSET = 6000
NAN = float('nan')

class PlotType(Enum):
    DAY = 1 
    GAP = 2

# def saturated_add()

## Gets datetime from timedelta (not recommended for mass use)
# def td2dt(timedelta):
#    return datetime.gmtime(timedelta.total_seconds())

## Makes abridged equivalent for TimeDelta of DateTime.timetuple; only includes d/h/m/s.
def td_tuple(td):
    days = td.days
    hours, remainder = divmod(td.seconds, 3600)
    mins, secs = divmod(remainder, 60)
    return (days, hours, mins, secs)

## Makes strftime for TimeDelta.
def td_strftime(td):
    (days, hours, mins, secs) = td_tuple(td)
    return "{d}{h}{m}{s}".format(d="{}d".format(days) if days != 0 else "", h="{}h".format(hours) if hours != 0 else "", m="{}m".format(mins) if mins != 0 else "", s="{}s".format(secs) if secs != 0 else "") # <-- terrifying code, please pacify :(

## Adds NANs to gap locations given deserialized gap string.
def amend_gaps(data, gaps):
    for gap in gaps:
        data.loc[gap - 0.5] = NAN
    data = data.sort_index().reset_index(drop=True)
    return data


## Retrieve dataset in proper format. May not work for non-HR, fix!!
def get_data(d_type, d_ind):
    return pd.read_csv("../data/{:03d}/{}_{:03d}.csv".format(d_ind, d_type, d_ind), parse_dates=['time'])


## MatPlotLib generate plot for either (a) per-day or (b) per-gap. Consuming operation.
def mpl(dat_ag, p_type, p_id): # <-- p_id is (a) day number or (b) gaps
    match p_type:
        case PlotType.DAY:
            ref_date = datetime.fromisoformat(str(dat_raw.iloc[0]['time'])[:10]) + dt.timedelta(days=p_id)
            ref_str = ref_date.strftime('%Y-%m-%d')
            dat = dat_ag[dat_ag['time'][:10] == ref_str]

        case PlotType.GAP:
            m_start = max(0, p_id - PT_GAP_MOD_OFFSET)
            m_end = p_id + PT_GAP_MOD_OFFSET #, sys.maxint)
            dat = dat_ag[m_start:m_end]

    print("Contents:", dat)

    plt.rcParams["figure.figsize"] = [7.00, 3.50]
    plt.rcParams["figure.autolayout"] = True
    plt.plot(dat['time'], dat['hr'], marker='o', markersize=1, linestyle='-', linewidth=1)

    dtf = DateFormatter('%Y-%m-%d %H:%M')
    plt.gca().xaxis.set_major_formatter(dtf)
    plt.gcf().autofmt_xdate()
    
    t_start = dat.iloc[0]['time']
    t_end = dat.iloc[-1]['time']
    delta_t = td_strftime(datetime.fromisoformat(str(t_end)) - datetime.fromisoformat(str(t_start)))

    plt.xlabel('Time')
    plt.ylabel('Heart Rate')
    plt.title("{} ▶ {} ({}) {}".format(t_start, t_end, delta_t, p_type))
    plt.grid(True, which='both', linestyle='--', linewidth=0.5)

    plt.tight_layout()
    print(f'Plotting {m_start} ▶ {p_id} ▶ {m_end} (type {p_type})')

    plt.show()
   

gaps = [ int(n) for n in sys.argv[1].split(";") ] # deserialize gap;gap;gap;...
dat_raw = get_data('HR', 1)
dat_ag = amend_gaps(dat_raw, gaps)

for gap in gaps:
    mpl(dat_ag, PlotType.GAP, gap)

mpl(dat_ag, PlotType.DAY, 1)
