using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Synth
{
    class Frequency
    {
        internal double hz
        {
            get;
            private set;
        }
        internal double volume
        {
            get;
            private set;
        }

        internal Frequency(double hz) : this(hz, 1.0) { }
        internal Frequency(double hz, double volume)
        {
            this.hz = hz;
            this.volume = volume;
        }
    }
}
