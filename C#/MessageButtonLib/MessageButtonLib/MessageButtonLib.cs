// MessageButtonLib.cs

using System;
using System.Drawing;
using System.Windows.Forms;

namespace Doug.ProgrammingWindowsForms
{
    public class MessageButton : Button
    {
        string myText;

        public MessageButton()
        {
            Enabled = false;
        }

        public string MessageBoxText
        {
            set
            {
                myText = value;
                Enabled = value != null && value.Length > 0;
            }
            get
            {
                return myText;
            }
        }

        protected override void OnClick(EventArgs e)
        {
            base.OnClick(e);
            MessageBox.Show(MessageBoxText, Text);
        }
    }
}