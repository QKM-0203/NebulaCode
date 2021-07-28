/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;

import error.ExecuteException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class Path extends Walkable {

    private List<Object> sequence = new ArrayList<>();

    public Path(List<Part> path){
          init(path);
    }

    public List<Object> getSequence() {
        return sequence;
    }

    /**
     * if is a right path
     */
    public void init(List<Part> path){
        sequence.add(path.get(0).getStartVertex());
        for (int index = 0; index < path.size(); index++) {
            Vertex lastVertex = (Vertex)sequence.get(sequence.size() - 1);
            if(lastVertex.getVid().equals(path.get(index).getStartVertex().getVid())){
                if(lastVertex.getVid().equals(path.get(index).getRelationship().getStartVertex())){
                    sequence.add(path.get(index).getRelationship());
                    if(path.get(index).getEndVertex().getVid().equals(path.get(index).getRelationship().getEndVertex())){
                        sequence.add(path.get(index).getEndVertex());
                    }else{
                        throw new ExecuteException(String.format("%s can not connect %s",
                                path.get(index).getRelationship().getEndVertex(),path.get(index).getEndVertex()));
                    }
                }else if(lastVertex.getVid().equals(path.get(index).getRelationship().getEndVertex())){
                    sequence.add(path.get(index).getRelationship());
                    if(path.get(index).getEndVertex().getVid().equals(path.get(index).getRelationship().getStartVertex())){
                        sequence.add(path.get(index).getEndVertex());
                    }else{
                        throw new ExecuteException(String.format("%s can not connect %s",
                                path.get(index).getRelationship().getStartVertex(),path.get(index).getEndVertex()));
                    }
                }else{
                    throw new ExecuteException(String.format("%s can not connect %s",
                            lastVertex,path.get(index).getStartVertex()));
                }
            }else{
                throw new ExecuteException(String.format("%s can not connect %s",
                        lastVertex,path.get(index).getStartVertex()));
            }
        }
        ArrayList<Vertex> vertices = new ArrayList<>();
        ArrayList<Relationship> relationships = new ArrayList<>();
        for (Object o : sequence) {
            if(o instanceof Vertex){
                vertices.add((Vertex) o);
            }else{
                relationships.add((Relationship) o);
            }
        }
        super.init(relationships,vertices);

    }

    /**
     * direct output
     */
    public void walk(){
        for (Object o : sequence) {
            System.out.println(o);
        }
    }


    /**
     * <("2" :QKM3{name: "asd", age: 19} :QKM2{name: "asd", age: 19})-[:asd@1{name: "asd", age: 19}]->("4" :QKM3{name: "asd", age: 19} :QKM2{name: "asd", age: 19})
     * -[:asd@1{name: "asd", age: 19}]->("5" :QKM3{name: "asd", age: 19} :QKM2{name: "asd", age: 19})<-[:asd@1{name: "asd", age: 19}]-("6" :QKM3{name: "asd", age: 19} :QKM2{name: "asd", age: 19})>
     * @return
     */
    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (int index = 0; index < sequence.size(); index++) {
            if(sequence.get(index) instanceof Vertex){
                result.append((Vertex)sequence.get(index));
            }else{
                ArrayList<String> prop = new ArrayList<>();
                StringBuilder part = new StringBuilder("{");
                HashMap<String, Object> propMap = ((Relationship) sequence.get(index)).getPropMap();
                for (String propName : ((Relationship) sequence.get(index)).getPropMap().keySet()) {
                    if(propMap.get(propName) instanceof  String){
                        prop.add(String.format("%s: "+"\""+"%s"+"\"",propName,propMap.get(propName)));
                    }else{
                        prop.add(String.format("%s: "+"%s",propName,propMap.get(propName)));
                    }
                }
                if(((Relationship)sequence.get(index)).getStartVertex().equals(((Vertex) sequence.get(index-1)).getVid())){

                    result.append(String.format("-" + "[:" +((Relationship) sequence.get(index)).getEdgeName()+
                                    "@"+((Relationship) sequence.get(index)).getRank() + "%s" + "]" + "->",
                            part.append(String.join(", ",prop)).append("}")));
                }else{
                    result.append(String.format("<-" + "[:" +((Relationship) sequence.get(index)).getEdgeName()+
                                    "@"+((Relationship) sequence.get(index)).getRank() + "%s" + "]" + "-",
                            part.append(String.join(", ",prop)).append("}")));
                }
            }
        }
        return String.format("<%s>",result);
    }
}
