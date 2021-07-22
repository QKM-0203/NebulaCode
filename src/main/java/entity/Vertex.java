package entity;


import java.util.*;


public class Vertex {

    //vertexVid
    private Object vid;


    private HashMap<String, HashMap<String,Object>> propMap;


    public Vertex(Object vid, HashMap<String, HashMap<String, Object>> propMap) {
        this.vid = vid;
        this.propMap = propMap;
    }

    public void setVid(Object vid) {
        this.vid = vid;
    }

    public void setPropMap(HashMap<String, HashMap<String, Object>> propMap) {
        this.propMap = propMap;
    }
}
