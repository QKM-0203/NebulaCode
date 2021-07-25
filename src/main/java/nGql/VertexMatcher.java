package nGql;


import entity.Graph;

import java.util.HashMap;

public class VertexMatcher extends VertexMatch {


    public VertexMatcher(Graph graph){
        super(graph);
    }

    public VertexMatch match(String tagName, HashMap<String,Object>propMap){
        init(tagName, propMap);
        return this;
    }

    public VertexMatch matchByVid(Object vid){
       return setVid(vid);
    }



}
