/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */
package entity;


import java.util.List;

public class Path  extends Walkable{


    public Path(List<Object> path){
          init(path);
          super.init(path);
    }


    //Convert to the form of alternating vertex and edges
    public void init(List<Object> path ){

    }

    public static List<String> walk(Walkable walkable){
        return null;
    }



}
