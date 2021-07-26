/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;



import java.util.List;

public class Subgraph{

    private List<Vertex> vertexList;

    private List<Relationship> relationshipsList;

    protected Subgraph(){

    }

    public Subgraph(List<Vertex> vertexList, List<Relationship> relationshipsList) {
          init(vertexList,relationshipsList);
    }

    protected void init(List<Vertex> vertexList, List<Relationship> relationshipsList){
        this.vertexList = vertexList;
        this.relationshipsList = relationshipsList;
        //Get nodes from inside edges and add them to vertexList
        //Judge that the subgraph cannot be empty. There must be at least one vertex
    }



    public  List<Vertex>  vertexes(){
        return  vertexList;
    }

    public List<Relationship> relationships(){
        return relationshipsList;
    }

    public Graph graph(){
        return vertexList.get(0).graph;
    }


    //{"Person", "Employee"}
    public List<String> tags(){
          return null;
    }

    //{"KNOWS", "LIKES", "DISLIKES","MARRIED_TO", "WORKS_FOR"}
    public List<String> types(){
        return null;
    }

    public List<Property> properties(){
        return null;
    }





}
