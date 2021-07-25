package nGql;


import entity.Graph;
import entity.Vertex;

import java.util.HashMap;

public class VertexMatcher extends VertexMatch {


    public VertexMatcher(Graph graph){
        super(graph);
    }

    public VertexMatch match(String tagName, HashMap<String,Object>propMap){
        init(tagName, propMap);
        return this;
    }

    public Vertex getVertexByVid(Object vid){
        return null;
    }



}
