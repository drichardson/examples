using System;
using System.Diagnostics;
using System.Drawing;
using System.Windows.Forms;
using System.Collections;

class LinkLabelDemo : Form
{
    [STAThread]
    public static void Main()
    {
        Application.EnableVisualStyles();
        Application.Run(new LinkLabelDemo());
    }

    LinkLabelDemo()
    {
        Text = "LinkLabel Demo";
        Font = new Font("Times New Roman", 14);

        LinkLabel ll = new LinkLabel();
        ll.Parent = this;
        ll.Dock = DockStyle.Fill;
        ll.LinkClicked += LinkLabelOnLinkClicked;
        ll.Text = "Jane Austen Societies exist in North America, the United Kingdom, and Australia, among other places.";

        Hashtable ht = new Hashtable();
        ht["North America"] = "http://www.jasna.org";
        ht["United Kingdom"] = "http://www.janeaustensoci.freeuk.com";
        ht["Australia"] = "http://www.jasa.net.au";

        foreach (DictionaryEntry entry in ht)
        {
            string region = (string)entry.Key;
            string url = (string)entry.Value;
            ll.Links.Add(ll.Text.IndexOf(region), region.Length, url);
        }
    }

    void LinkLabelOnLinkClicked(object objSrc, LinkLabelLinkClickedEventArgs args)
    {
        LinkLabel.Link lnk = args.Link;
        string strLink = lnk.LinkData as string;

        Process.Start(strLink);
    }
}