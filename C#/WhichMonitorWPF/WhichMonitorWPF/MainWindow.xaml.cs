using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using System.Runtime.InteropServices;

namespace WhichMonitorWPF
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        private void Button_Click_Resize_Win32(object sender, RoutedEventArgs e)
        {
            var window = new System.Windows.Interop.WindowInteropHelper(this);
            IntPtr hWnd = window.Handle;
            IntPtr hMonitor = Win32.MonitorFromWindow(hWnd, Win32.MONITOR_DEFAULTTONEAREST);

            var monitorInfo = new Win32.MONITORINFOEX();
            monitorInfo.cbSize = (int)Marshal.SizeOf(monitorInfo);
            if (Win32.GetMonitorInfo(hMonitor, ref monitorInfo))
            {
                Console.WriteLine("GetMonitorInfo succeeded. " + monitorInfo.szDevice);

                Left = monitorInfo.rcWork.Left;
                Top = monitorInfo.rcWork.Top;
                Width = monitorInfo.rcWork.Right - monitorInfo.rcWork.Left;
                Height = monitorInfo.rcWork.Bottom - monitorInfo.rcWork.Top;
            }
            else
            {
                Console.WriteLine("GetMonitorInfo failed." + monitorInfo.szDevice);
            }
        }

        private void Button_Click_Resize_Forms(object sender, RoutedEventArgs e)
        {
            Console.WriteLine("Button_Click_Forms");

            var window = new System.Windows.Interop.WindowInteropHelper(this);
            IntPtr hWnd = window.Handle;
            var screen = System.Windows.Forms.Screen.FromHandle(hWnd);

            Left = screen.WorkingArea.Left;
            Top = screen.WorkingArea.Top;
            Width = screen.WorkingArea.Width;
            Height = screen.WorkingArea.Height;
        }

        private void Button_Click_SetMaxHeightTo50Percent_Win32(object sender, RoutedEventArgs e)
        {
            var window = new System.Windows.Interop.WindowInteropHelper(this);
            IntPtr hWnd = window.Handle;
            IntPtr hMonitor = Win32.MonitorFromWindow(hWnd, Win32.MONITOR_DEFAULTTONEAREST);

            var monitorInfo = new Win32.MONITORINFOEX();
            monitorInfo.cbSize = (int)Marshal.SizeOf(monitorInfo);
            if (Win32.GetMonitorInfo(hMonitor, ref monitorInfo))
            {
                Console.WriteLine("GetMonitorInfo succeeded. " + monitorInfo.szDevice);
                MaxHeight = (monitorInfo.rcWork.Bottom - monitorInfo.rcWork.Top) * 0.5;
            }
            else
            {
                Console.WriteLine("GetMonitorInfo failed." + monitorInfo.szDevice);
            }
        }

        private void Button_Click_SetMaxHeightTo50Percent_Forms(object sender, RoutedEventArgs e)
        {
            var window = new System.Windows.Interop.WindowInteropHelper(this);
            IntPtr hWnd = window.Handle;
            var screen = System.Windows.Forms.Screen.FromHandle(hWnd);

            MaxHeight = 0.5 * screen.WorkingArea.Height;
        }

        private void Window_SizeChanged(object sender, SizeChangedEventArgs e)
        {
            // Window_SizeChange is just for debugging to see what's happening to the window size.
            Console.WriteLine($"Size Changed: Left=#{Left} Top=#{Top} Width=#{Width} Height=#{Height})");
        }
    }
}
