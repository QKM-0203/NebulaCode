/* Copyright (c) 2021 vesoft inc. All rights reserved.
 *
 * This source code is licensed under Apache 2.0 License,
 * attached with Common Clause Condition 1.0, found in the LICENSES directory.
 */

package ngql;

import entity.Vertex;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * used to splice objects into nGql statements.
 *
 * @author Qi Kai Meng
 */
public class Encoding {
    /**
     * splice the tags in the map into tagString,eg:player(name,age).
     *
     * @param propMap tagMap
     * @return tagString spliced by tagMap
     */
    public static String vertexTagJoin(HashMap<String, HashMap<String, Object>> propMap) {
        ArrayList<String> tagStringList = new ArrayList<>();
        for (String tagName : propMap.keySet()) {
            String tagPart = tagName + "(%s)";
            HashMap<String, Object> propValueMap = propMap.get(tagName);
            ArrayList<String> propNameList = null;
            String tagPartFormat = null;
            if (propValueMap != null) {
                propNameList = new ArrayList<>(propValueMap.keySet());
                tagPartFormat = String.format(tagPart, String.join(",", propNameList));
            } else {
                tagPartFormat = String.format(tagPart, "");
            }
            tagStringList.add(tagPartFormat);
        }
        return String.join(",", tagStringList);
    }

    public static String vertexValueJoin(Vertex vertex) {
        Object vid = vertex.getVid();
        StringBuilder values = new StringBuilder();
        String result = String.format("%s:(%s)",
            (vid instanceof String) ? "\"" + vid + "\"" : vid,"%s");
        HashMap<String, HashMap<String, Object>> propMap = vertex.getPropMap();
        for (String tagName : propMap.keySet()) {
            HashMap<String, Object> propValueMap = propMap.get(tagName);
            for (String propName : propValueMap.keySet()) {

            }
        }
        return String.format(result,values);
    }
}
