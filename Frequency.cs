using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Synth
{
    class Frequency
    {
        internal protected double hz
        {
            get;
            protected set;
        }
        internal protected double volume
        {
            get;
            protected set;
        }

        internal Frequency(double hz) : this(hz, 1.0) { }
        internal Frequency(double hz, double volume)
        {
            this.hz = hz;
            this.volume = volume;
        }
        protected Frequency() { } // used to extend in Note
    }
}
