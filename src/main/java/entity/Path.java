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

    public Path(List<Relationship> relationships){
          init(relationships);
    }


    /**
     * if is a right path
     */
    public void init(List<Relationship> relationships){
        ArrayList<Vertex> vertices = new ArrayList<>();
        sequence.add(relationships.get(0).getStartVertex());
        sequence.add(relationships.get(0));
        sequence.add(relationships.get(0).getEndVertex());
        for (int index = 1; index < relationships.size(); index++) {
            Vertex vertex = (Vertex)sequence.get(sequence.size() - 1);
            if(vertex.equals(relationships.get(index).getStartVertex())){
                sequence.add(relationships.get(index));
                sequence.add(relationships.get(index).getEndVertex());

            }else if(vertex.equals(relationships.get(index).getEndVertex())){
                sequence.add(relationships.get(index));
                sequence.add(relationships.get(index).getStartVertex());
            }else{
                throw new ExecuteException(String.format("%s can not connect %s",(Vertex)sequence.get(sequence.size()-1)
                        ,(Relationship)relationships.get(index)));
            }
        }
        for (Object o : sequence) {
            if(o instanceof Vertex){
                vertices.add((Vertex) o);
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
                if(((Relationship)sequence.get(index)).getStartVertex().equals((Vertex) sequence.get(index-1))){

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
