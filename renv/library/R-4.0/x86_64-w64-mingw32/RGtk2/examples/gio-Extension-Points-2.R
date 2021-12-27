## Implement an extension point
myExampleImplType <- gClass("MyExampleImpl", MY_TYPE_EXAMPLE)
gIoExtensionPointImplement ("my-extension-point",
                            myExampleImplType,
                            "my-example",
                            10);
