package com.example1.myfirstapp;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.view.Window;
import android.view.WindowManager;

public class MainActivity extends Activity
{
    public final static String EXTRA_MESSAGE = "com.example.myfirstapp.MESSAGE";

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
        //focusEdit();
    }

    @Override
    public void onStart() {
        super.onStart();
        System.out.println("DOUG: onStart");
    }

    @Override
    public void onResume() {
        super.onResume();
        System.out.println("DOUG: onResume");
    }

    public void sendMessage(View view) {
        System.out.println("DOUG: sendMessage called");
        Intent intent = new Intent(this, DisplayMessageActivity.class);
        EditText editText = (EditText)findViewById(R.id.edit_message);
        System.out.println("DOUG: editText is " + editText);
        String message = editText.getText().toString();
        intent.putExtra(EXTRA_MESSAGE, message);
        startActivity(intent);
    }


    /*
    private void focusEdit() {
        Window w = getWindow();
        System.out.println("window is " + w);
        w.setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);

        System.out.println("DOUG: Current focus is " + getCurrentFocus());
        EditText t = (EditText)findViewById(R.id.edit_message);
        System.out.println("DOUG: EditText t = " + t);
        if (t.requestFocus()) {
            InputMethodManager imm = (InputMethodManager)getSystemService(Context.INPUT_METHOD_SERVICE);
            System.out.println("DOUG: 2 imm is " + imm);
            boolean r = imm.showSoftInput(t, InputMethodManager.SHOW_IMPLICIT);
            System.out.println("DOUG: r is " + r);
        } else {
            System.out.println("DOUG: requestFocus returned false");
        }
        System.out.println("DOUG: 3 Current focus is " + getCurrentFocus());
    }
    */


}
