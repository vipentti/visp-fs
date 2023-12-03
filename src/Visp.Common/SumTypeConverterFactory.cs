// Copyright 2023 Ville Penttinen
// Distributed under the MIT License.
// https://github.com/vipentti/visp-fs/blob/main/LICENSE.md

using System.Reflection;
using System.Text.Json;
using System.Text.Json.Serialization;
using Microsoft.VisualStudio.LanguageServer.Protocol;

namespace Visp.Common;

public class SumTypeConverterFactory : JsonConverterFactory
{
    public override bool CanConvert(Type typeToConvert)
    {
        if (!typeToConvert.IsGenericType)
        {
            return false;
        }

        if (typeToConvert.GetGenericTypeDefinition() != typeof(SumType<,>))
        {
            return false;
        }

        return true;
    }

    public override JsonConverter? CreateConverter(Type type, JsonSerializerOptions options)
    {
        var keyType = type.GetGenericArguments()[0];
        var valueType = type.GetGenericArguments()[1];

        var converter = (JsonConverter)
            Activator.CreateInstance(
                typeof(SumTypeConverterInner<,>).MakeGenericType([keyType, valueType]),
                BindingFlags.Instance | BindingFlags.Public,
                binder: null,
                args: [options],
                culture: null
            )!;

        return converter;
    }

    private class SumTypeConverterInner<TFirst, TSecond>(JsonSerializerOptions options)
        : JsonConverter<SumType<TFirst, TSecond>>
    {
        private readonly Type _firstType = typeof(TFirst);
        private readonly Type _secondType = typeof(TSecond);
        private readonly JsonConverter<TFirst> _firstConverter =
            (JsonConverter<TFirst>)options.GetConverter(typeof(TFirst));
        private readonly JsonConverter<TSecond> _secondConverter =
            (JsonConverter<TSecond>)options.GetConverter(typeof(TSecond));

        public override SumType<TFirst, TSecond> Read(
            ref Utf8JsonReader reader,
            Type typeToConvert,
            JsonSerializerOptions options
        )
        {
            try
            {
                // copy reader
                var temp = reader;
                var first = _firstConverter.Read(ref temp, _firstType, options);
                return first ?? throw new JsonException();
            }
            catch
            {
                var second = _secondConverter.Read(ref reader, _secondType, options);
                return second ?? throw new JsonException();
            }
        }

        public override void Write(
            Utf8JsonWriter writer,
            SumType<TFirst, TSecond> value,
            JsonSerializerOptions options
        )
        {
            if (value.TryGetFirst(out var first))
            {
                _firstConverter.Write(writer, first, options);
            }
            else if (value.TryGetSecond(out var second))
            {
                _secondConverter.Write(writer, second, options);
            }
            else
            {
                throw new JsonException();
            }
        }
    }
}
