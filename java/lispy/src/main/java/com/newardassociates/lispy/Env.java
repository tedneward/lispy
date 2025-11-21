package com.newardassociates.lispy;

import java.util.HashMap;
import java.util.Map;

/**
 * "An environment: a dict of {'var':val} pairs, with an outer Env."
 * 
 * Corresponds directly to lis.py[82:89]
 * 
 * @author Ted Neward
 */
public class Env {
    private Map<String, Object> env = new HashMap<>();
    private Env outer = null;

    public Env(String[] parms, Object[] args, Env outer) {
        this.outer = outer;
        for (int i = 0; i < parms.length; i++) {
            this.env.put(parms[i], args[i]);
        }
    }
    public Env(Env outer) {
        this.outer = outer;
    }
    public Env() { }

    public void set(String key, Object value) {
        // Always look local first before checking
        // outer scope(s)
        //
        if (this.env.containsKey(key)) {
            this.env.put(key, value);
            return;
        }

        if (this.outer != null && this.outer.env.containsKey(key)) {
            this.outer.env.put(key, value);
            return;
        }

        this.env.put(key, value);
    }

    public Object get(String key) {
        // Always look local first before checking
        // outer scope(s)
        //
        if (this.env.containsKey(key))
            return this.env.get(key);

        if (this.outer != null)
            return this.outer.get(key);

        return null;
    }    
}
