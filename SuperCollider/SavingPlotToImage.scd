// Run these one at a time slowly, otherwise it will crash due to
//

p = { SinOsc.ar() }.plot;
i = Image.fromWindow(p.parent, p.interactionView.bounds);
i.write("~/SuperColliderPlotOutput.png".standardizePath);
i.free;

