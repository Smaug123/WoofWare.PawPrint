using System;
using System.Threading;

namespace HelloWorldApp
{
    // Array-backed queue with a deliberate ordering bug in Enqueue: it
    // bumps _count BEFORE populating the slot. A concurrent Dequeuer
    // that observes _count > 0 in the gap finds _items[_head] still null.
    // System.Collections.Generic.Queue<T> does these writes in the
    // opposite order (slot, then ++_size) and so does not exhibit this
    // specific race even though it is not thread-safe in general.
    sealed class BadQueue<T> where T : class
    {
        private readonly T[] _items;
        private int _head;
        private int _tail;
        private int _count;

        public BadQueue(int capacity)
        {
            _items = new T[capacity];
        }

        public int Count
        {
            get { return _count; }
        }

        public void Enqueue(T item)
        {
            _count++;
            _items[_tail] = item;
            int next = _tail + 1;
            if (next == _items.Length) next = 0;
            _tail = next;
        }

        public T Dequeue()
        {
            if (_count == 0) throw new InvalidOperationException("empty queue");
            T item = _items[_head];
            if (item == null) throw new InvalidOperationException("torn enqueue: Count > 0 but slot was empty");
            _items[_head] = null;
            int next = _head + 1;
            if (next == _items.Length) next = 0;
            _head = next;
            _count--;
            return item;
        }
    }

    class Program
    {
        static BadQueue<object> queue = new BadQueue<object>(16);

        static void Worker1()
        {
            queue.Enqueue(new object());
        }

        static void Worker2()
        {
            while (true)
            {
                if (queue.Count > 0)
                {
                    queue.Dequeue();
                    return;
                }
            }
        }

        static int Main(string[] args)
        {
            Thread t1 = new Thread(Worker1);
            Thread t2 = new Thread(Worker2);
            t1.Start();
            t2.Start();
            t1.Join();
            t2.Join();
            return 0;
        }
    }
}
