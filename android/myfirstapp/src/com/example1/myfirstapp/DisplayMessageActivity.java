package com.example1.myfirstapp;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
//import android.support.v7.app.ActionBarActivity;
import android.view.MenuItem;
import android.view.Menu;
import android.view.MenuInflater;
import android.widget.EditText;
import android.widget.TextView;

public class DisplayMessageActivity extends Activity
{
    protected void onCreate(Bundle savedInstanceState)
    {
        System.out.println("DMA howdy");
        super.onCreate(savedInstanceState);
        Intent intent = getIntent();
        String message = intent.getStringExtra(MainActivity.EXTRA_MESSAGE);

        TextView textView = new TextView(this);
        textView.setTextSize(40);
        textView.setText(message);

        setContentView(textView);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu)
    {
        getMenuInflater().inflate(R.menu.main_activity_actions, menu);
        return super.onCreateOptionsMenu(menu);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item)
    {
        switch(item.getItemId()) {
            case R.id.action_search:
                openSearch();
                return true;
            case R.id.action_settings:
                openSettings();
                return true;
        }

        return super.onOptionsItemSelected(item);
    }

    void openSearch()
    {

        System.out.println("DOUG: openSearch");
    }

    void openSettings()
    {
        System.out.println("DOUG: openSettings");
    }

}

