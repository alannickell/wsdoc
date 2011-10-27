/*
 * Copyright 2011 TaskDock, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.versly.rest.wsdoc;

import org.apache.commons.lang.StringUtils;
import org.apache.ecs.html.*;
import org.codehaus.jackson.annotate.JsonIgnore;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import javax.annotation.processing.AbstractProcessor;
import javax.annotation.processing.RoundEnvironment;
import javax.annotation.processing.SupportedAnnotationTypes;
import javax.lang.model.element.*;
import javax.lang.model.type.*;
import javax.tools.Diagnostic;
import javax.tools.FileObject;
import javax.tools.StandardLocation;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.*;
import java.util.Map;

/**
 * Generates an HTML documentation file describing the REST / JSON endpoints as defined with the
 * Spring {@link org.springframework.web.bind.annotation.RequestMapping} annotation. Outputs to
 * <code>rest-api.html</code> in the top of the classes directory.
 */
// TODO:
//   - @CookieValue
//   - @RequestHeader
//   - @ResponseStatus
//   - combine class-level and method-level annotations properly
//   - MethodNameResolver
//   - plural RequestMapping value support (i.e., two paths bound to one method)
//   - support for methods not marked with @RequestMapping whose class does have a @RequestMapping annotation
@SupportedAnnotationTypes("org.springframework.web.bind.annotation.RequestMapping")
public class AnnotationProcessor extends AbstractProcessor {

    private RestDocumentation _docs = new RestDocumentation();
    private boolean _isComplete = false;

    @Override
    public boolean process(Set<? extends TypeElement> supportedAnnotations, RoundEnvironment roundEnvironment) {

        // short-circuit if there are multiple rounds
        if (_isComplete) { return true; }

        Collection<String> processedPackageNames = new LinkedHashSet<String>();
        for (Element e : roundEnvironment.getElementsAnnotatedWith(RequestMapping.class)) {
            if (e instanceof ExecutableElement) {
                addPackageName(processedPackageNames, e);
                processRequestMappingMethod((ExecutableElement) e);
            }
        }

        if (_docs.getResources().size() > 0) {

            OutputStream fout = null;
            try {
                FileObject file = getOutputFile();
                boolean exists = new File(file.getName()).exists();
                fout = file.openOutputStream();
                _docs.toStream(fout);
                processingEnv.getMessager().printMessage(Diagnostic.Kind.NOTE,
                                                         String.format("Wrote REST docs for %s endpoints to %s file at %s",
                                                                       _docs.getResources().size(), exists ? "existing" : "new", file.getName()));
            } catch (Exception e) {
                throw new RuntimeException(e); // TODO wrap in something nicer
            } finally {
                if (fout != null) {
                    try {
                        fout.close();
                    } catch (IOException ignored) {
                        // ignored
                    }
                }
            }
        }
        _isComplete = true;
        return true;
    }

    private void addPackageName(Collection<String> processedPackageNames, Element e) {
        processedPackageNames.add(processingEnv.getElementUtils().getPackageOf(e).getQualifiedName().toString());
    }

    private FileObject getOutputFile() throws IOException {
        return this.processingEnv.getFiler().createResource(StandardLocation.CLASS_OUTPUT, "", Utils.SERIALIZED_RESOURCE_LOCATION);
    }

    private void processRequestMappingMethod(ExecutableElement executableElement) {
        TypeElement cls = (TypeElement) executableElement.getEnclosingElement();
        String path = getClassLevelUrlPath(cls);

        RequestMapping anno = executableElement.getAnnotation(RequestMapping.class);
        path = addMethodPathComponent(executableElement, cls, path, anno);
        RequestMethod meth = getRequestMethod(executableElement, cls, anno);

        RestDocumentation.Resource.Method doc = _docs.getResourceDocumentation(path).newMethodDocumentation(meth);
        String docComment = processingEnv.getElementUtils().getDocComment(executableElement);
        if (StringUtils.isNotBlank(docComment)) {
            MethodStructure methodStructure = generateMethodStructure(docComment);
            doc.setCommentText(generateJavaDocHTML(methodStructure));
            doc.setCommentSummary(methodStructure.getDescription());
        }
        buildParameterData(executableElement, doc);
        buildResponseFormat(executableElement.getReturnType(), doc);
    }

    private static MethodStructure generateMethodStructure(String docComment) {
        MethodStructure methodStructure = new MethodStructure();

        parseNameDescriptionObj(docComment, 0, methodStructure, "@param", new ParseProcessor() {
            @Override public void processEntry(MethodStructure methodStructure, String... args) {
                methodStructure.addParam(args[0], args[1]);
            }
        });
        parseNameDescriptionObj(docComment, 0, methodStructure, "@throws", new ParseProcessor() {
            @Override public void processEntry(MethodStructure methodStructure, String... args) {
                methodStructure.addException(args[0], args[1]);
            }
        });
        parseValueObj(docComment, methodStructure, "@return", new ParseProcessor() {
            @Override public void processEntry(MethodStructure methodStructure, String... args) {
                methodStructure.setReturns(args[0]);
            }
        });
        parseValueObj(docComment, methodStructure, "@deprecated", new ParseProcessor() {
            @Override public void processEntry(MethodStructure methodStructure, String... args) {
                if (args[0] != null && !args[0].isEmpty()) {
                    methodStructure.setDeprecated(true);
                    methodStructure.setDeprecatedMessage(args[0]);
                }
            }
        });
        parseValueObj(docComment, methodStructure, "@since", new ParseProcessor() {
            @Override public void processEntry(MethodStructure methodStructure, String... args) {
                methodStructure.setSince(args[0]);
            }
        });
        parseValueObj(docComment, methodStructure, "@see", new ParseProcessor() {
            @Override public void processEntry(MethodStructure methodStructure, String... args) {
                methodStructure.setSeeAlso(args[0]);
            }
        });
        methodStructure.setDescription(docComment.substring(0, (docComment.indexOf("@") != -1 ? docComment.indexOf("@") : docComment.length())));
        return methodStructure;
    }

    private static String generateJavaDocHTML(MethodStructure methodStructure) {
        DL dl = new DL();

        //
        // Deprecated
        //
        if (methodStructure.isDeprecated()) {
            DD deprecatedDD = new DD();

            B b = new B("Deprecated. ");
            deprecatedDD.addElement(b);

            I i = new I(methodStructure.getDeprecatedMessage());
            deprecatedDD.addElement(i);
            deprecatedDD.addElement(new P());

            dl.addElement(deprecatedDD);
        }

        //
        // Description
        //
        DD descriptionDD = new DD();
        P descriptionP = new P(methodStructure.getDescription(), "");
        descriptionDD.addElement(descriptionP);
        descriptionDD.addElement(new P());
        dl.addElement(descriptionDD);

        DD extraInfoDD = new DD();
        DL extraInfoDL = new DL();
        extraInfoDD.addElement(extraInfoDL);

        //
        // Params
        //
        if (!methodStructure.getParams().isEmpty()) {
            DT dt = new DT();
            dt.addElement(new B("Parameters:"));
            extraInfoDL.addElement(dt);
            for (Map.Entry<String, String> paramMap : methodStructure.getParams().entrySet()) {
                DD paramDD = new DD();
                Code paramName = new Code(paramMap.getKey());
                paramDD.addElement(paramName);
                paramDD.addElement(" - " + paramMap.getValue());
                extraInfoDL.addElement(paramDD);
            }
        }

        //
        // Returns
        //
        if (StringUtils.isNotBlank(methodStructure.getReturns())) {
            DT dt = new DT();
            dt.addElement(new B("Returns:"));
            extraInfoDL.addElement(dt);
            extraInfoDL.addElement(new DD(methodStructure.getReturns()));
        }

        //
        // Throws
        //
        if (!methodStructure.getExceptions().isEmpty()) {
            DT dt = new DT();
            dt.addElement(new B("Throws:"));
            extraInfoDL.addElement(dt);
            for (Map.Entry<String, String> exceptionMap : methodStructure.getExceptions().entrySet()) {
                DD exceptionDD = new DD();
                Code exceptionName = new Code(exceptionMap.getKey());
                exceptionDD.addElement(exceptionName);
                exceptionDD.addElement(" - " + exceptionMap.getValue());
                extraInfoDL.addElement(exceptionDD);
            }
        }

        //
        // Since
        //
        if (StringUtils.isNotBlank(methodStructure.getSince())) {
            DT dt = new DT();
            dt.addElement(new B("Since:"));
            extraInfoDL.addElement(dt);
            extraInfoDL.addElement(new DD(methodStructure.getSince()));
        }

        //
        // See
        //
        if (StringUtils.isNotBlank(methodStructure.getSeeAlso())) {
            DT dt = new DT();
            dt.addElement(new B("See Also:"));
            extraInfoDL.addElement(dt);
            extraInfoDL.addElement(new DD(methodStructure.getSeeAlso()));
        }

        dl.addElement(extraInfoDD);

        return dl.toString();
    }

    private static interface ParseProcessor {
        void processEntry(MethodStructure methodStructure, String... args);
    }

    private static void parseNameDescriptionObj(String docComment, int startIdx, MethodStructure methodStructure, String annotation, ParseProcessor processor) {
        int annotationBegin = docComment.indexOf(annotation, startIdx);
        if (annotationBegin == -1) {
            return;
        }
        int nameStart = docComment.indexOf(" ", annotationBegin) + 1;
        int nameEnd = docComment.indexOf(" ", nameStart);
        if (nameEnd > nameStart) {
            String name = docComment.substring(nameStart, nameEnd);

            int descriptionEnd = docComment.indexOf("@", nameEnd) - 1;

            String description = "";
            if (descriptionEnd > nameEnd) {
                description = docComment.substring(nameEnd + 1, descriptionEnd);
            }

            processor.processEntry(methodStructure, name, description);

            if (descriptionEnd > 0) {
                parseNameDescriptionObj(docComment, descriptionEnd, methodStructure, annotation, processor);
            }
        }
    }

    private static void parseValueObj(String docComment, MethodStructure methodStructure, String annotation, ParseProcessor processor) {
        int annotationBegin = docComment.indexOf(annotation);
        if (annotationBegin == -1) {
            return;
        }
        int valStart = annotationBegin + annotation.length() + 1;
        int valEnd = docComment.indexOf("@", valStart);
        if (valEnd == -1) {
            valEnd = docComment.length();
        }
        if (valEnd > valStart) {
            String val = docComment.substring(valStart, valEnd);

            processor.processEntry(methodStructure, val);
        }
    }

    private static final class MethodStructure {
        private Map<String, String> params;
        private String returns;
        private String since;
        private Map<String, String> exceptions;
        private String seeAlso;
        private boolean deprecated;
        private String deprecatedMessage;
        private String description;

        private MethodStructure() {
            this.params = new HashMap<String, String>();
            this.exceptions = new HashMap<String, String>();
        }

        public Map<String, String> getParams() {
            return params;
        }

        public void addParam(String name, String description) {
            this.params.put(name, description);
        }

        public String getReturns() {
            return returns;
        }

        public void setReturns(String returns) {
            this.returns = returns;
        }

        public String getSince() {
            return since;
        }

        public void setSince(String since) {
            this.since = since;
        }

        public Map<String, String> getExceptions() {
            return exceptions;
        }

        public void addException(String name, String description) {
            this.exceptions.put(name, description);
        }

        public String getSeeAlso() {
            return seeAlso;
        }

        public void setSeeAlso(String seeAlso) {
            this.seeAlso = seeAlso;
        }

        public boolean isDeprecated() {
            return deprecated;
        }

        public void setDeprecated(boolean deprecated) {
            this.deprecated = deprecated;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(String description) {
            this.description = description;
        }

        public String getDeprecatedMessage() {
            return deprecatedMessage;
        }

        public void setDeprecatedMessage(String deprecatedMessage) {
            this.deprecatedMessage = deprecatedMessage;
        }
    }

    private void buildParameterData(ExecutableElement executableElement, RestDocumentation.Resource.Method doc) {

        // only process @RequestBody, @PathVariable and @RequestParam parameters for now.
        // TODO Consider expanding this to include other Spring REST annotations.

        // We can safely ignore @RequestMapping.params, as Spring requires that a @RequestParam exists
        // for each entry listed in this list. I expect that this might be the same for @RequestMapping.headers

        scanForMultipart(executableElement, doc);
        buildPathVariables(executableElement, doc);
        buildUrlParameters(executableElement, doc);
        buildRequestBodies(executableElement, doc);
    }

    private void scanForMultipart(ExecutableElement executableElement, RestDocumentation.Resource.Method doc) {
        for (VariableElement var : executableElement.getParameters()) {
            TypeMirror varType = var.asType();
            if (varType.toString().startsWith(MultipartHttpServletRequest.class.getName())) {
                doc.setMultipartRequest(true);
                return;
            }
        }
    }

    private void buildRequestBodies(ExecutableElement executableElement, RestDocumentation.Resource.Method doc) {
        List<VariableElement> requestBodies = new ArrayList<VariableElement>();
        for (VariableElement var : executableElement.getParameters()) {
            if (var.getAnnotation(org.springframework.web.bind.annotation.RequestBody.class) != null) {
                requestBodies.add(var);
            }
        }

        if (requestBodies.size() > 1) {
            throw new IllegalStateException(String.format(
                    "Method %s in class %s has multiple @RequestBody params",
                    executableElement.getSimpleName(), executableElement.getEnclosingElement()));
        }

        if (requestBodies.size() == 1) { buildRequestBody(requestBodies.get(0), doc); }
    }

    private void buildRequestBody(VariableElement var, RestDocumentation.Resource.Method doc) {
        doc.setRequestBody(newJsonType(var.asType()));
    }

    private void buildPathVariables(ExecutableElement executableElement, RestDocumentation.Resource.Method doc) {
        RestDocumentation.Resource.Method.UrlFields subs = doc.getUrlSubstitutions();

        for (VariableElement var : executableElement.getParameters()) {
            PathVariable pathVar = var.getAnnotation(PathVariable.class);
            if (pathVar != null) {
                addUrlField(subs, var, pathVar.value());
            }
        }
    }

    private void addUrlField(RestDocumentation.Resource.Method.UrlFields subs, VariableElement var, String annoValue) {
        String name = (annoValue == null || annoValue.isEmpty()) ? var.getSimpleName().toString() : annoValue;
        subs.addField(name, newJsonType(var.asType()));
    }

    private void buildUrlParameters(ExecutableElement executableElement, RestDocumentation.Resource.Method doc) {
        RestDocumentation.Resource.Method.UrlFields subs = doc.getUrlParameters();

        for (VariableElement var : executableElement.getParameters()) {
            RequestParam reqParam = var.getAnnotation(RequestParam.class);
            if (reqParam != null) {
                addUrlField(subs, var, reqParam.value());
            }
        }
    }

    private JsonType newJsonType(TypeMirror typeMirror) {
        if (isJsonPrimitive(typeMirror)) {
            return new JsonPrimitive(typeMirror.toString());
        } else if (typeMirror.getKind() == TypeKind.DECLARED) {
            // some sort of object... walk it
            DeclaredType type = (DeclaredType) typeMirror;
            return newJsonType(type, type.getTypeArguments(), new HashSet<DeclaredType>());
        } else if (typeMirror.getKind() == TypeKind.VOID) {
            return null;
        } else if (typeMirror.getKind() == TypeKind.ARRAY) {
            return newJsonType(((ArrayType) typeMirror).getComponentType());
        } else {
            throw new UnsupportedOperationException(typeMirror.toString());
        }
    }

    /**
     * Create a new JSON type for the given declared type. The caller is responsible for
     * providing a list of concrete types to use to replace parameterized type placeholders.
     */
    private JsonType newJsonType(DeclaredType type, List<? extends TypeMirror> concreteTypes,
                                 Collection<DeclaredType> typeRecursionGuard) {
        TypeVisitorImpl visitor
                = new TypeVisitorImpl(type, concreteTypes, typeRecursionGuard);
        return type.accept(visitor, null);
    }

    private boolean isJsonPrimitive(TypeMirror typeMirror) {
        return (typeMirror.getKind().isPrimitive()
                || JsonPrimitive.isPrimitive(typeMirror.toString()));
    }

    private void buildResponseFormat(TypeMirror type, RestDocumentation.Resource.Method doc) {
        doc.setResponseBody(newJsonType(type));
    }

    private RequestMethod getRequestMethod(ExecutableElement executableElement, TypeElement cls, RequestMapping anno) {
        if (anno.method().length != 1) {
            throw new IllegalStateException(String.format(
                    "The RequestMapping annotation for %s.%s is not parseable. Exactly one request method (GET/POST/etc) is required.",
                    cls.getQualifiedName(), executableElement.getSimpleName()));
        } else { return anno.method()[0]; }
    }

    private String addMethodPathComponent(ExecutableElement executableElement, TypeElement cls, String path, RequestMapping anno) {
        if (anno == null || anno.value().length != 1) {
            throw new IllegalStateException(String.format(
                    "The RequestMapping annotation for %s.%s is not parseable. Exactly one value is required.",
                    cls.getQualifiedName(), executableElement.getSimpleName()));
        } else { return Utils.joinPaths(path, anno.value()[0]); }
    }

    private String getClassLevelUrlPath(TypeElement cls) {
        RestApiMountPoint mountPoint = cls.getAnnotation(RestApiMountPoint.class);
        String path = mountPoint == null ? "/" : mountPoint.value();

        RequestMapping clsAnno = cls.getAnnotation(RequestMapping.class);
        if (clsAnno == null || clsAnno.value().length == 0) { return path; } else if (clsAnno.value().length == 1) {
            return Utils.joinPaths(path, clsAnno.value()[0]);
        } else {
            throw new IllegalStateException(String.format(
                    "The RequestMapping annotation of class %s has multiple value strings. Only zero or one value is supported",
                    cls.getQualifiedName()));
        }
    }

    private class TypeVisitorImpl implements TypeVisitor<JsonType, Void> {
        private Map<Name, DeclaredType> _typeArguments = new HashMap();
        private Collection<DeclaredType> _typeRecursionGuard;
        private DeclaredType _type;

        public TypeVisitorImpl(DeclaredType type, List<? extends TypeMirror> typeArguments,
                               Collection<DeclaredType> typeRecursionGuard) {

            TypeElement elem = (TypeElement) type.asElement();
            _typeRecursionGuard = typeRecursionGuard;
            _type = type;
            List<? extends TypeParameterElement> generics = elem.getTypeParameters();
            for (int i = 0; i < generics.size(); i++) {
                DeclaredType value =
                        (typeArguments.isEmpty() || !(typeArguments.get(i) instanceof DeclaredType)) ?
                        null : (DeclaredType) typeArguments.get(i);
                _typeArguments.put(generics.get(i).getSimpleName(), value);
            }
        }

        @Override
        public JsonType visit(TypeMirror typeMirror, Void o) {
            throw new UnsupportedOperationException(typeMirror.toString());
        }

        @Override
        public JsonType visit(TypeMirror typeMirror) {
            throw new UnsupportedOperationException(typeMirror.toString());
        }

        @Override
        public JsonType visitPrimitive(PrimitiveType primitiveType, Void o) {
            return newJsonType(primitiveType);
        }

        @Override
        public JsonType visitNull(NullType nullType, Void o) {
            throw new UnsupportedOperationException(nullType.toString());
        }

        @Override
        public JsonType visitArray(ArrayType arrayType, Void o) {
            throw new UnsupportedOperationException(arrayType.toString());
        }

        @Override
        public JsonType visitDeclared(DeclaredType declaredType, Void o) {
            if (isJsonPrimitive(declaredType)) {
                // 'primitive'-ish things
                return new JsonPrimitive(declaredType.toString());

            } else if (isInstanceOf(declaredType, Collection.class)) {

                if (declaredType.getTypeArguments().size() == 0) {
                    return new JsonArray(new JsonPrimitive(Object.class.getName()));
                } else {
                    TypeMirror elem = declaredType.getTypeArguments().get(0);
                    return new JsonArray(elem.accept(this, o));
                }

            } else if (isInstanceOf(declaredType, Map.class)) {

                if (declaredType.getTypeArguments().size() == 0) {
                    return new JsonDict(
                            new JsonPrimitive(Object.class.getName()), new JsonPrimitive(Object.class.getName()));
                } else {
                    TypeMirror key = declaredType.getTypeArguments().get(0);
                    TypeMirror val = declaredType.getTypeArguments().get(1);
                    return new JsonDict(key.accept(this, o), val.accept(this, o));
                }

            } else {
                TypeElement element = (TypeElement) declaredType.asElement();
                if (element.getKind() == ElementKind.ENUM) {
                    List<String> enumConstants = new ArrayList();
                    for (Element e : element.getEnclosedElements()) {
                        if (e.getKind() == ElementKind.ENUM_CONSTANT) {
                            enumConstants.add(e.toString());
                        }
                    }
                    JsonPrimitive primitive = new JsonPrimitive(String.class.getName());  // TODO is this always a string?
                    primitive.setRestrictions(enumConstants);
                    return primitive;
                } else {
                    return buildType(declaredType, element);
                }
            }
        }

        private JsonType buildType(DeclaredType declaredType, TypeElement element) {
            if (_typeRecursionGuard.contains(declaredType)) {
                return new JsonRecursiveObject(element.getSimpleName().toString());
            }

            JsonObject json = new JsonObject();
            buildTypeContents(json, element);
            return json; // we've already added to the cache; short-circuit to handle recursion
        }

        private boolean isInstanceOf(TypeMirror typeMirror, Class type) {
            if (!(typeMirror instanceof DeclaredType)) { return false; }

            if (typeMirror.toString().startsWith(type.getName())) { return true; }

            DeclaredType declaredType = (DeclaredType) typeMirror;
            TypeElement typeElement = (TypeElement) declaredType.asElement();
            for (TypeMirror iface : typeElement.getInterfaces()) {
                if (isInstanceOf(iface, type)) { return true; }
            }
            return isInstanceOf(typeElement.getSuperclass(), type);
        }

        private void buildTypeContents(JsonObject o, TypeElement element) {
            if ("org.springframework.web.servlet.ModelAndView".equals(element.getQualifiedName().toString())) {
                return;
            }
            if (element.getSuperclass().getKind() != TypeKind.NONE) {
                // an interface's superclass is TypeKind.NONE

                DeclaredType sup = (DeclaredType) element.getSuperclass();
                if (!isJsonPrimitive(sup)) { buildTypeContents(o, (TypeElement) sup.asElement()); }
            }

            for (Element e : element.getEnclosedElements()) {
                if (e instanceof ExecutableElement) {
                    addFieldFromBeanMethod(o, (ExecutableElement) e);
                }
            }
        }

        private void addFieldFromBeanMethod(JsonObject o, ExecutableElement executableElement) {
            if (!isJsonBeanGetter(executableElement)) { return; }

            TypeMirror type = executableElement.getReturnType();
            String methodName = executableElement.getSimpleName().toString();
            int trimLength = methodName.startsWith("is") ? 2 : 3;
            String beanName = methodName.substring(trimLength + 1, methodName.length());
            beanName = methodName.substring(trimLength, trimLength + 1).toLowerCase() + beanName;

            // loop over the element's generic types, and build a concrete list from the owning context
            List<DeclaredType> concreteTypes = new ArrayList();

            // replace variables with the current concrete manifestation
            if (type instanceof TypeVariable) {
                type = getDeclaredTypeForTypeVariable((TypeVariable) type);
                if (type == null) {
                    return; // couldn't find a replacement -- must be a generics-capable type with no generics info
                }
            }

            String docComment = processingEnv.getElementUtils().getDocComment(executableElement);
            if (type instanceof DeclaredType) {
                TypeElement element = (TypeElement) ((DeclaredType) type).asElement();
                for (TypeParameterElement generic : element.getTypeParameters()) {
                    concreteTypes.add(_typeArguments.get(generic.getSimpleName()));
                }
                Collection<DeclaredType> types = new HashSet<DeclaredType>(_typeRecursionGuard);
                types.add(_type);
                o.addField(beanName, newJsonType((DeclaredType) type, concreteTypes, types))
                        .setCommentText(docComment);
            } else {
                o.addField(beanName, newJsonType(type))
                        .setCommentText(docComment);
            }
        }

        private boolean isJsonBeanGetter(ExecutableElement executableElement) {
            if (executableElement.getKind() != ElementKind.METHOD) { return false; }

            if (executableElement.getReturnType().getKind() == TypeKind.NULL) { return false; }

            if (!(executableElement.getSimpleName().toString().startsWith("get")
                  || executableElement.getSimpleName().toString().startsWith("is"))) { return false; }

            if (executableElement.getParameters().size() > 0) { return false; }

            return executableElement.getAnnotation(JsonIgnore.class) == null;
        }

        @Override
        public JsonType visitError(ErrorType errorType, Void o) {
            throw new UnsupportedOperationException(errorType.toString());
        }

        @Override
        public JsonType visitTypeVariable(TypeVariable typeVariable, Void o) {
            DeclaredType type = getDeclaredTypeForTypeVariable(typeVariable);
            if (type != null) // null: un-parameterized usage of a generics-having type
            { return type.accept(this, o); } else { return null; }
        }

        private DeclaredType getDeclaredTypeForTypeVariable(TypeVariable typeVariable) {
            Name name = typeVariable.asElement().getSimpleName();
            if (!_typeArguments.containsKey(name)) {
                throw new UnsupportedOperationException(String.format(
                        "Unknown parameterized type: %s. Available types in this context: %s",
                        typeVariable.toString(), _typeArguments));
            } else {
                return _typeArguments.get(name);
            }
        }

        @Override
        public JsonType visitWildcard(WildcardType wildcardType, Void o) {
            throw new UnsupportedOperationException(wildcardType.toString());
        }

        @Override
        public JsonType visitExecutable(ExecutableType executableType, Void o) {
            throw new UnsupportedOperationException(executableType.toString());
        }

        @Override
        public JsonType visitNoType(NoType noType, Void o) {
            throw new UnsupportedOperationException(noType.toString());
        }

        @Override
        public JsonType visitUnknown(TypeMirror typeMirror, Void o) {
            throw new UnsupportedOperationException(typeMirror.toString());
        }
    }
}
