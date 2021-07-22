package entity;

import java.util.List;

public class Walkable {

   private Subgraph subgraph;

    public Walkable(Subgraph subgraph) {
        this.subgraph = subgraph;
    }

    public void setSubgraph(Subgraph subgraph) {
        this.subgraph = subgraph;
    }

    //get id or all
    public Vertex getStartNode(){
        return null;
    }

    public Vertex getEndNode(){
        return null;
    }

    //get all nodes of subgraph
    public List<Vertex> getNodes(){
        return null;
    }

    //get all relationships of subgraph
    public List<Relationship> getRelationships(){
        return null;
    }



}
