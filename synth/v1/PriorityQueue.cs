using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Synth
{
    internal class PriorityQueue<T>
    {
        private readonly List<KeyValuePair<double, T>> backingList = new List<KeyValuePair<double, T>>();

        internal void Push(double weight, T item)
        {
            KeyValuePair<double, T> pair = new KeyValuePair<double, T>(weight, item);

            for (int i = 0; i < backingList.Count; i++)
            {
                if (backingList[i].Key > weight)
                {
                    backingList.Insert(i, pair);
                    return;
                }
            }

            // base case: it's the highest (or only) weight
            backingList.Insert(backingList.Count, pair);
        }

        internal T Peek()
        {
            if (backingList.Count == 0)
                return default(T);

            KeyValuePair<double, T> item = backingList[0];
            return item.Value;
        }

        internal T Pop()
        {
            if (backingList.Count == 0)
                return default(T);

            KeyValuePair<double, T> item = backingList[0];
            backingList.RemoveAt(0);
            return item.Value;
        }

        internal int Count
        {
            get
            {
                return backingList.Count;
            }
        }
    }
}
