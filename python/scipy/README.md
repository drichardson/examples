# SciPy Examples

1. Install Python 3.8+ with tkinter support. On Windows that means you cannot
   use the Python distribution from the Microsoft Store, which tkinter is not
   included. Instead, [download](https://www.python.org/downloads/windows/) the
   [full installer](https://docs.python.org/3/using/windows.html#windows-full).
2. *Optional* Create and activate a virtual environment with
   [venv](https://docs.python.org/3/library/venv.html) with:

   Windows (PowerShell):

   ```PowerShell
   python -m venv myenv
   myenv\Scripts\Activate.ps1
   ```

   Debian 10 (Buster):

   ```bash
   sudo apt-get install python3-pip python3-venv
   python3 -m venv myenv
   source myenv/bin/activate
   ```

3. Install prerequisites:

   ```bash
   pip install -r requirements.txt
   ```

