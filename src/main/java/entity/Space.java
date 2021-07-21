package entity;

import Operator.Type;

public class Space {

    private String spaceName;

    //图空间的分片数量
    private int partitionNumber = 100;

    //每个分片的副本数量
    private int replicaFactor = 1;

    //ID的数据类型可选值FIXED_STRING(<N>)和INT64
    private Type vidType = Type.FIXED_STRING;

    public Space(String spaceName, int partitionNumber, int replicaFactor, Type vidType) {
        this.spaceName = spaceName;
        this.partitionNumber = partitionNumber;
        this.replicaFactor = replicaFactor;
        this.vidType = vidType;
    }

    public Space() {

    }

    public String getSpaceName() {
        return spaceName;
    }

    public void setSpaceName(String spaceName) {
        this.spaceName = spaceName;
    }


    public void setPartitionNumber(int partitionNumber) {
        this.partitionNumber = partitionNumber;
    }


    public void setReplicaFactor(int replicaFactor) {
        this.replicaFactor = replicaFactor;
    }



    public void setVidType(Type vidType) {
        this.vidType = vidType;
    }
}